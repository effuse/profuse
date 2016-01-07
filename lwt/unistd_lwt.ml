
open Lwt

let stub_read = 

let read fd ptr len =
  if len < 0 then invalid_arg "Unistd_lwt.read"
  else Lwt_unix.blocking fd
    >>= function
    | true ->
      Lwt_unix.wait_read fd >>= fun () ->
      Lwt_unix.run_job (read_job ch.fd ptr len)
    | false ->
      Lwt_unix.(wrap_syscall Read) fd (fun () -> stub_read ch.fd ptr len)
