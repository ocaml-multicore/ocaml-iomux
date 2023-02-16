type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Raw = struct
  external poll : buffer -> int -> int -> int = "caml_iomux_poll"
  external ppoll : buffer -> int -> float -> int list -> int = "caml_iomux_ppoll"
  external set_index : buffer -> int -> int -> int -> unit = "caml_iomux_poll_set_index" [@@noalloc]
  external get_revents : buffer -> int -> int = "caml_iomux_poll_get_revents" [@@noalloc]
  external get_fd : buffer -> int -> int = "caml_iomux_poll_get_fd" [@@noalloc]
  external max_open_files : unit -> int = "caml_iomux_poll_max_open_files" [@@noalloc]
end

module Flags = struct
  type t = int

  let pollin = Config.pollin
  let pollout = Config.pollout
  let pollerr = Config.pollerr
  let pollhup = Config.pollhup
  let pollnval = Config.pollnval

  let empty = 0

  let ( + ) = ( lor )

  let mem a b = (a land b) <> 0
end

let fd_of_unix fd = (Obj.magic fd : int)

let unix_of_fd (fd : int) : Unix.file_descr = (Obj.magic fd)

let invalid_fd = unix_of_fd (-1)

let max_open_files = Raw.max_open_files

type t = {
  buffer : buffer;
  maxfds : int;
}

type timeout =
  | Infinite
  | Nowait
  | Milliseconds of int

let poll t used timeout =
  let timeout = match timeout with
    | Infinite -> (-1)
    | Nowait -> 0
    | Milliseconds ms -> ms
  in
  Raw.poll t.buffer used timeout

type ppoll_timeout =
  | Infinite
  | Nowait
  | Timeout of float

let ppoll t used timeout sigmask =
  let timeout = match timeout with
    | Infinite -> (-1.0)
    | Nowait -> 0.
    | Timeout timo -> timo
  in
  Raw.ppoll t.buffer used timeout sigmask

let guard_index t index =
  if index >= t.maxfds then
    invalid_arg "index out of bounds"

let set_index t index fd events =
  guard_index t index;
  Raw.set_index t.buffer index (fd_of_unix fd) events

let invalidate_index t index =
  guard_index t index;
  Raw.set_index t.buffer index (-1) 0

let get_revents t index =
  guard_index t index;
  Raw.get_revents t.buffer index

let get_fd t index =
  guard_index t index;
  Raw.get_fd t.buffer index |> unix_of_fd

let create ?(maxfds=max_open_files ()) () =
  let len = maxfds * Config.sizeof_pollfd in
  let buffer = Bigarray.(Array1.create char c_layout len) in
  { buffer; maxfds }

let iter_ready t nready (f : int -> Unix.file_descr -> int -> unit) =
  let rec loop index nready =
    match nready with
    | 0 -> ()
    | _ ->
      let fd = get_fd t index in
      let revents = get_revents t index in
      if fd <> invalid_fd && revents <> 0 then (
        f index fd revents;
        loop (succ index) (pred nready)
      ) else
        loop (succ index) nready
  in
  loop 0 nready
