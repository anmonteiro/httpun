open Mirage

(* Network configuration *)

let stack = generic_stackv4 default_network

(* Dependencies *)

let server =
 let packages =
   [ package ~pin:"file://../../" "httpun-lwt"
   ; package ~pin:"file://../../" "httpun-mirage"
   ]
  in
  foreign "Unikernel.Make"
    ~packages
    (console @-> pclock @-> http @-> job)

let app =
  httpun_server @@ conduit_direct stack

let () =
  register "httpun_unikernel"
    [ server $ default_console $ default_posix_clock $ app ]
