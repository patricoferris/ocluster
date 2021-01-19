type t

module Sandbox = Obuilder.Sandbox

module Config : sig
  type t
  
  val v : Sandbox.config -> [ `Zfs of string option * string | `Btrfs of string ] -> t

  module Sandbox : Obuilder.S.SANDBOX
end

val create : ?prune_threshold:float -> Config.t -> t Lwt.t

val build : t ->
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  spec:Obuilder.Spec.t ->
  src_dir:string -> (string, [ `Cancelled | `Msg of string ]) Lwt_result.t
