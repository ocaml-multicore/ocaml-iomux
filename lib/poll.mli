module Flags :
  sig
    type t = int
    val pollin : int
    val pollout : int
    val pollerr : int
    val pollhup : int
    val pollnval : int
    val empty : int
    val ( + ) : int -> int -> int
    val mem : int -> int -> bool
  end

val invalid_fd : Unix.file_descr

type t

val create : ?maxfds:int -> unit -> t

type poll_timeout = Infinite | Nowait | Milliseconds of int

val poll : t -> int -> poll_timeout -> int

type ppoll_timeout = Infinite | Nowait | Nanoseconds of int64

val ppoll : t -> int -> ppoll_timeout -> int list -> int

val set_index : t -> int -> 'a -> int -> unit

val invalidate_index : t -> int -> unit

val get_revents : t -> int -> int

val get_fd : t -> int -> Unix.file_descr

val iter_ready : t -> int -> (int -> Unix.file_descr -> int -> unit) -> unit
