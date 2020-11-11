open Astring
open Lwt.Infix
open Capnp_rpc_lwt

type Sqlite_loader.Ty.t += Client
let () = Sqlite_loader.Ty.register "ocluster-client" Client

let restart_timeout = 600.0   (* Maximum time to wait for a worker to reconnect after it disconnects. *)

module Item = struct
  type t = {
    descr : Cluster_api.Queue.job_desc;
    set_job : Cluster_api.Raw.Service.Job.t Capability.resolver;
  }

  type cache_hint = string

  let default_estimate = S.{
      cached = 10;                (* A build with cached dependencies usually only takes about 10 seconds. *)
      non_cached = 600;           (* If we have to install dependencies, it'll probably take about 10 minutes. *)
  }

  let cost_estimate _t = default_estimate

  let cache_hint t =
    Cluster_api.Raw.Reader.JobDescr.cache_hint_get t.descr

  let pp f t =
    match cache_hint t with
    | "" -> Fmt.string f "(no cache hint)"
    | x -> Fmt.string f x
end

module Pool_api = struct
  module Pool = Pool.Make(Item)(Unix)

  module Client_map = Map.Make(String)

  type t = {
    pool : Pool.t;
    mutable clients : Pool.Client.t Client_map.t;
    workers : (string, Cluster_api.Worker.t) Hashtbl.t;
    cond : unit Lwt_condition.t;    (* Fires when a worker joins *)
  }

  let create ~name ~db =
    let pool = Pool.create ~name ~db in
    let clients = Client_map.empty in
    let workers = Hashtbl.create 10 in
    let cond = Lwt_condition.create () in
    { pool; clients; workers; cond }

  let client t ~client_id =
    match Client_map.find_opt client_id t.clients with
    | Some c -> c
    | None ->
      let client = Pool.client t.pool ~client_id in
      t.clients <- Client_map.add client_id client t.clients;
      client

  let submit t ~urgent ~client_id (descr : Cluster_api.Queue.job_desc) : Cluster_api.Ticket.t =
    let client = client t ~client_id in
    let job, set_job = Capability.promise () in
    Log.info (fun f -> f "Received new job request from %S (urgent=%b)" client_id urgent);
    let item = { Item.descr; set_job } in
    let ticket = Pool.submit ~urgent client item in
    let cancel () =
      match Pool.cancel ticket with
      | Ok () ->
        Capability.resolve_exn set_job (Capnp_rpc.Exception.v "Ticket cancelled");
        Lwt_result.return ()
      | Error `Not_queued ->
        Cluster_api.Job.cancel job
    in
    let release () =
      match Pool.cancel ticket with
      | Ok () -> Capability.resolve_exn set_job (Capnp_rpc.Exception.v "Ticket released (cancelled)")
      | Error `Not_queued -> ()
    in
    Cluster_api.Ticket.local ~job ~cancel ~release

  let pop q ~job =
    Pool.pop q >|= function
    | Error `Finished -> Error (`Capnp (Capnp_rpc.Error.exn "Worker disconnected"))
    | Ok { set_job; descr } ->
      Capability.inc_ref job;
      Capability.resolve_ok set_job job;
      Ok descr

  let register t ~name ~capacity worker =
    match Pool.register t.pool ~name ~capacity with
    | Error `Name_taken ->
      Fmt.failwith "Worker already registered!";
    | Ok q ->
      Pool.set_active q true;
      Log.info (fun f -> f "Registered new worker %S" name);
      Hashtbl.add t.workers name worker;
      Lwt_condition.broadcast t.cond ();
      Cluster_api.Queue.local
        ~pop:(pop q)
        ~set_active:(Pool.set_active q)
        ~release:(fun () ->
          Hashtbl.remove t.workers name;
          Capability.dec_ref worker;
          Pool.release q
        )

  let registration_service t =
    let register = register t in
    Cluster_api.Registration.local ~register

  let worker t name =
    match Hashtbl.find_opt t.workers name with
    | None -> None
    | Some w ->
      Capability.inc_ref w;
      Some w

  let admin_service t =
    let show () = Fmt.to_to_string Pool.show t.pool in
    let workers () =
      Pool.connected_workers t.pool
      |> Astring.String.Map.bindings
      |> List.map (fun (name, worker) ->
          let active = Pool.is_active worker in
          { Cluster_api.Pool_admin.name; active }
        )
    in
    let set_active name active =
      match Astring.String.Map.find_opt name (Pool.connected_workers t.pool) with
      | Some worker -> Pool.set_active worker active; Ok ()
      | None -> Error `Unknown_worker
    in
    let update name =
      match Astring.String.Map.find_opt name (Pool.connected_workers t.pool) with
      | None -> Service.fail "Unknown worker"
      | Some w ->
        let cap = Option.get (worker t name) in
        Pool.shutdown w;        (* Prevent any new items being assigned to it. *)
        Service.return_lwt @@ fun () ->
        Capability.with_ref cap @@ fun worker ->
        Log.info (fun f -> f "Restarting %S" name);
        Cluster_api.Worker.self_update worker >>= function
        | Error _ as e -> Lwt.return e
        | Ok () ->
          Log.info (fun f -> f "Waiting for %S to reconnect after update" name);
          let rec aux () =
            match Astring.String.Map.find_opt name (Pool.connected_workers t.pool) with
            | Some new_w when new_w != w -> Lwt_result.return (Service.Response.create_empty ())
            | _ -> Lwt_condition.wait t.cond >>= aux
          in
          let timeout = 
            Lwt_unix.sleep restart_timeout >|= fun () ->
            Error (`Capnp (Capnp_rpc.Error.exn "Timeout waiting for worker to reconnect!"))
          in
          Lwt.pick [ aux (); timeout ]
    in
    Cluster_api.Pool_admin.local ~show ~workers ~worker:(worker t) ~set_active ~update
end

type t = {
  pools : Pool_api.t String.Map.t;
}

let registration_services t =
  String.Map.map Pool_api.registration_service t.pools |> String.Map.bindings

let pp_pool_name f (name, _) = Fmt.string f name

let submission_service ~validate ~sturdy_ref t client_id =
  let submit ~pool ~urgent descr =
    match validate () with
    | false -> Capability.broken (Capnp_rpc.Exception.v "Access has been revoked")
    | true ->
      match String.Map.find_opt pool t.pools with
      | None ->
        let msg = Fmt.strf "Pool ID %S not one of @[<h>{%a}@]" pool (String.Map.pp ~sep:Fmt.comma pp_pool_name) t.pools in
        Capability.broken (Capnp_rpc.Exception.v msg)
      | Some pool ->
        Pool_api.submit ~urgent ~client_id pool descr
  in
  Cluster_api.Submission.local ~submit ~sturdy_ref

let pool t name =
  String.Map.find_opt name t.pools

let admin_service ~loader ~restore t =
  let pools () = String.Map.bindings t.pools |> List.map fst in
  let pool name =
    match String.Map.find_opt name t.pools with
    | None -> Capability.broken (Capnp_rpc.Exception.v "No such pool")
    | Some pool_api -> Pool_api.admin_service pool_api
  in
  let add_client name =
    let descr = (Client, name) in
    match Sqlite_loader.lookup_by_descr loader descr with
    | [] ->
      let secret = Sqlite_loader.add loader descr in
      Log.info (fun f -> f "Created new submission endpoint for client %S" name);
      Capnp_rpc_net.Restorer.restore restore secret
      >|= Result.map_error (fun x -> `Capnp (`Exception x))
    | _ -> Lwt_result.fail (`Capnp (Capnp_rpc.Error.exn "Client %S already registered!" name))
  in
  let remove_client name =
    let descr = (Client, name) in
    match Sqlite_loader.lookup_by_descr loader descr with
    | [digest] ->
      Sqlite_loader.remove loader digest;
      Log.info (fun f -> f "Removed endpoint for client %S" name);
      Lwt_result.return ()
    | [] -> Lwt_result.fail (`Capnp (Capnp_rpc.Error.exn "Unknown client %S" name))
    | _ -> failwith "BUG: multiple users found with the same ID!"
  in
  let list_clients () =
    Sqlite_loader.list_by_type loader Client |> List.map (fun (_digest, name) -> name) |> Lwt.return
  in
  Cluster_api.Admin.local ~pools ~pool ~add_client ~remove_client ~list_clients

let create ~db pools =
  let db = Pool.Dao.init db in
  let pools =
    List.fold_left
      (fun acc name -> String.Map.add name (Pool_api.create ~name ~db) acc)
      String.Map.empty pools
  in
  { pools }

module S = S
module Pool = Pool
module Sqlite_loader = Sqlite_loader
