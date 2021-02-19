open Lwt.Infix

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
    Fmt.failwith "Expected two lines from df, but got:@,%S" lines

(* Df doesn't seem to report accurate measures for ZFS
   https://pavelanni.github.io/oracle_solaris_11_labs/zfs/zfs_free/ *)
let zfs_free_space pool = 
  Lwt_process.pread ("", [| "zpool"; "list"; "-H"; "-o"; "cap"; pool|]) >|= fun used ->  
  try 100. -. Scanf.sscanf used " %f%%" Fun.id
  with _ -> Fmt.failwith "Expected %S, got %S" "xx%" used