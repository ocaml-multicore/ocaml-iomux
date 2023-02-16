(* [@@@warning "-32"] *)
exception Fdleak

let _check_raises = Alcotest.check_raises
let _check_string = Alcotest.(check string)
let _check_int = Alcotest.(check int)
let _check_bool = Alcotest.(check bool)

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
    let _pollfds = Iomux.Poll.create () in
    ()

  let () =
    let open Alcotest in
    let wlc = U.with_leak_checker in
    run "Iomux" [
      "unit",                  [ test_case "" `Quick (wlc basic) ];
    ]

end
