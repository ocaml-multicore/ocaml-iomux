open Iomux

exception Fdleak

let _check_raises = Alcotest.check_raises
let _check_string = Alcotest.(check string)
let check_int = Alcotest.(check int)
let check_bool = Alcotest.(check bool)

module U = struct
  let with_leak_checker (f : unit -> unit) () =
    let fetch () =
      let l = List.init 512 (fun _ -> Unix.(socket PF_UNIX SOCK_STREAM 0)) in
      List.iter Unix.close l;
      l
    in
    let l1 = fetch () in
    match f () with
    | exception exn -> raise exn
    | () ->
      (* Linux is buggy. In multithreaded programs not always the
         file-descriptor is released immediatelly if it has/have been
         used in another thread. This causes the list to be
         re-ordered, with sometimes one file descriptor showing up
         only later (but it shows up so it's not a leak). So we just
         fetch again. *)
        if (l1 <> fetch ()) && (l1 <> fetch ()) then
          raise Fdleak

  let _coinflip () = Random.bool ()

end

module T = struct

  let basic () =
    let poll = Iomux.Poll.create ~maxfds:16 () in
    let r, w = Unix.pipe () in
    Poll.set_index poll 0 r Poll.Flags.pollin;
    let b = Bytes.create 1 in
    check_int "write" (Unix.write w b 0 1) 1;
    let nready = Poll.poll poll 1 Nowait in
    check_int "nready" nready 1;
    let fd = Poll.get_fd poll 0 in
    let revents = Poll.get_revents poll 0 in
    check_bool "fd" true (r = fd);
    check_bool "revents" true (Poll.Flags.mem revents Poll.Flags.pollin);
    check_bool "revents-eq" true (revents = Poll.Flags.pollin);
    Unix.close w;
    Unix.close r

  let ppoll_timo () =
    let pollfds = Iomux.Poll.create () in
    let one_second = 1000_000_000L in
    ignore @@ Iomux.Poll.ppoll pollfds 0 (Nanoseconds one_second) []

  let () =
    let open Alcotest in
    let wlc = U.with_leak_checker in
    run "Iomux" [
      "unit",                  [ test_case "" `Quick (wlc basic) ];
      "ppoll_timo",            [ test_case "" `Quick (wlc ppoll_timo) ];
    ]

end
