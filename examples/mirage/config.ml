open Mirage

(* Network configuration *)

let stack = generic_stackv4 default_network

(* Dependencies *)

let server =
 let packages =
   [ package ~pin:"file://../../" "httpaf-lwt"
   ; package ~pin:"file://../../" "httpaf-mirage"
   ]
  in
  foreign "Unikernel.Make"
    ~packages
    (console @-> pclock @-> http @-> job)

let app =
  httpaf_server @@ conduit_direct stack

let () =
  register "httpaf_unikernel"
    [ server $ default_console $ default_posix_clock $ app ]
