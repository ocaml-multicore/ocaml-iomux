(*
 * Copyright (c) 2023 Christiano Haesbaert <haesbaert@haesbaert.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Util

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Raw = struct
  external poll : buffer -> int -> int -> int = "caml_iomux_poll"
  external ppoll : buffer -> int -> int64 -> int list -> int = "caml_iomux_ppoll"
  external set_index : buffer -> int -> int -> int -> unit = "caml_iomux_poll_set_index" [@@noalloc]
  external init : buffer -> int -> unit = "caml_iomux_poll_init"
  external get_revents : buffer -> int -> int = "caml_iomux_poll_get_revents" [@@noalloc]
  external get_fd : buffer -> int -> int = "caml_iomux_poll_get_fd" [@@noalloc]
end

module Flags = struct
  type t = int

  let pollin = Config.pollin
  let pollpri = Config.pollpri
  let pollout = Config.pollout
  let pollerr = Config.pollerr
  let pollhup = Config.pollhup
  let pollnval = Config.pollnval

  let empty = 0

  let ( + ) = ( lor )

  let mem a b = (a land b) <> 0

  let to_int = Fun.id
  let of_int = Fun.id
end

let has_ppoll = Config.has_ppoll

let has_kqueue = Kqueue.available

let invalid_fd = unix_of_fd (-1)

type kqueue = {
  kq : Kqueue.t;
  changelist : Kqueue.Event_list.t;
  mutable eventlist : Kqueue.Event_list.t;
}

type fds =
  | Poll of buffer
  | Kqueue of kqueue

let get_poll = function
  | Poll b -> b
  | Kqueue _ -> assert false

type t = {
  buffer : fds;
  maxfds : int;
}

type poll_timeout =
  | Infinite
  | Nowait
  | Milliseconds of int

let poll t used timeout =
  let timeout = match timeout with
    | Infinite -> (-1)
    | Nowait -> 0
    | Milliseconds ms -> ms
  in
  Raw.poll (get_poll t.buffer) used timeout

type ppoll_timeout =
  | Infinite
  | Nowait
  | Nanoseconds of int64

let ppoll t used timeout sigmask =
  let timeout = match timeout with
    | Infinite -> Int64.minus_one
    | Nowait -> Int64.zero
    | Nanoseconds timo -> timo
  in
  Raw.ppoll (get_poll t.buffer) used timeout sigmask

let kqueue k nfds timeout =
  let timeout = match timeout with
    | Infinite -> Kqueue.Timeout.never
    | Nowait -> Kqueue.Timeout.immediate
    | Nanoseconds timo -> Kqueue.Timeout.of_ns timo
  in
  let eventlist = if nfds = 0 then Kqueue.Event_list.null else Kqueue.Event_list.create nfds in
  let n = Kqueue.kevent k.kq ~changelist:Kqueue.Event_list.null ~eventlist timeout in
  k.eventlist <- eventlist;
  n

let ppoll_or_poll_or_kqueue t used (timeout : ppoll_timeout) =
  match t.buffer with
  | Kqueue k -> kqueue k used timeout
  | Poll _ ->
    if has_ppoll then
      ppoll t used timeout []
    else
      let timeout : poll_timeout = match timeout with
        | Infinite -> Infinite
        | Nowait -> Nowait
        | Nanoseconds timo_ns ->
          Milliseconds (Int64.(to_int @@ div (add timo_ns 999_999L) 1_000_000L))
      in
      poll t used timeout

let guard_index t index =
  if index >= t.maxfds || index < 0 then
    invalid_arg "index out of bounds"

let set_index t index fd events =
  guard_index t index;
  match t.buffer with
  | Kqueue k ->
    let changelist = Kqueue.Event_list.create 1 in
    let ev1 = Kqueue.Event_list.get changelist 0 in
    let filter =
      if Flags.(mem events pollin) then Kqueue.Filter.read
      else Kqueue.Filter.write
    in
    let ev2 = Kqueue.Event_list.get k.changelist index in
    List.iter (fun ev ->
    Kqueue.Event_list.Event.set_ident ev (Kqueue.Util.file_descr_to_int fd);
    Kqueue.Event_list.Event.set_filter ev filter;
    Kqueue.Event_list.Event.set_flags ev Kqueue.Flag.add) [ ev1; ev2 ];
    let v : int = Kqueue.kevent k.kq ~changelist ~eventlist:Kqueue.Event_list.null Kqueue.Timeout.immediate in
    assert (v = 0)
  | Poll buffer -> Raw.set_index buffer index (fd_of_unix fd) events

let invalidate_index t index =
  guard_index t index;
  match t.buffer with
  | Kqueue k ->
    let ev = Kqueue.Event_list.get k.changelist index in
    Kqueue.Event_list.Event.set_flags ev Kqueue.Flag.delete
  | Poll buffer ->
    Raw.set_index buffer index (-1) 0

let kqueue_filter_to_poll f =
  if Kqueue.Filter.(f = read) then Flags.pollin
  else Flags.pollout

let get_revents t index =
  guard_index t index;
  match t.buffer with
  | Kqueue k ->
    let ev = Kqueue.Event_list.get k.eventlist index in
    Kqueue.Event_list.Event.get_filter ev |> kqueue_filter_to_poll
  | Poll buffer ->
    Raw.get_revents buffer index

let get_fd t index =
  guard_index t index;
  match t.buffer with
  | Kqueue k ->
    let ev = Kqueue.Event_list.get k.eventlist index in
    Kqueue.Event_list.Event.get_ident ev |> Kqueue.Util.file_descr_of_int
  | Poll buffer ->
    Raw.get_fd buffer index |> unix_of_fd

let create ?(maxfds=Util.max_open_files ()) () =
  let len = maxfds * Config.sizeof_pollfd in
  let buffer =
    if has_kqueue
    then
      let eventlist = Kqueue.Event_list.create 1 in
      let changelist = Kqueue.Event_list.create maxfds in
      let kq = { kq = Kqueue.create (); eventlist; changelist } in
      Kqueue kq
    else Poll (Bigarray.(Array1.create char c_layout len))
  in
  let t = { buffer; maxfds } in
  Raw.init buffer maxfds;
  t

let maxfds t = t.maxfds

let iter_ready t nready (f : int -> Unix.file_descr -> Flags.t -> unit) =
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
