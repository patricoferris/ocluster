let free_space_percent path = 
  let float = Int64.to_float in 
  let s = ExtUnix.All.statvfs path in 
  let total = float s.f_blocks in
  let available = float s.f_bavail in
  (* let roots = float s.f_bfree in
  let used = total -. roots in
  let non_root = used +. available in *)
  let free = 100. *. (total /. available) in
  Log.info (fun f -> f "Free Space: %f\n" free);
    Lwt.return free
(* open Lwt.Infix

let free_space_percent path =
  Lwt_process.pread ("", [| "df"; path; "--output=pcent" |]) >|= fun lines ->
  match String.split_on_char '\n' (String.trim lines) with
  | [_; result] ->
    let used =
      try Scanf.sscanf result " %f%%" Fun.id
      with _ -> Fmt.failwith "Expected %S, got %S" "xx%" result
    in
    100. -. used
  | _ ->
    Fmt.failwith "Expected two lines from df, but got:@,%S" lines *)
