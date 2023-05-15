module C = Configurator.V1

let has_ppoll_code = {|
#define _GNU_SOURCE /* for linux */
#include <poll.h>
#include <stddef.h>
#include <strings.h>

int
main(void)
{
	struct pollfd fds;
	struct timespec ts;

	bzero(&fds, sizeof(fds));
	bzero(&ts, sizeof(ts));

	return (ppoll(&fds, 0, &ts, NULL));
}
|}

let has_epoll_code = {|
#include <sys/epoll.h>
#include <err.h>

int
main(void)
{
	int efd;

	efd = epoll_create(10);
	if (efd == -1)
		err(1, "epoll_create");
	close(efd);

	return (0);
}
|}

let () =
  C.main ~name:"discover" @@ fun c ->

  (* check for ppoll(2) *)
  let has_ppoll = C.c_test c has_ppoll_code in
  let has_epoll = C.c_test c has_epoll_code in
  C.C_define.gen_header_file c ~fname:"config.h" [ ("HAS_PPOLL", Switch has_ppoll);
                                                   ("HAS_EPOLL", Switch has_epoll) ];
  let has_list = [ Printf.sprintf "let has_ppoll = %b" has_ppoll;
                   Printf.sprintf "let has_epoll = %b" has_epoll ] in

  (* general poll(2) definitions *)
  let poll_defs =
    C.C_define.import c ~includes:["poll.h"]
      C.C_define.Type.[
        "POLLIN", Int;
        "POLLPRI", Int;
        "POLLOUT", Int;
        "POLLERR", Int;
        "POLLHUP", Int;
        "POLLNVAL", Int;
        "sizeof(struct pollfd)", Int;
      ]
  in
  let epoll_defs = if not has_epoll then [] else
      C.C_define.import c ~includes:["sys/epoll.h"]
        C.C_define.Type.[
          "EPOLLIN", Int;
          "EPOLLPRI", Int;
          "EPOLLOUT", Int;
          "EPOLLRDNORM", Int;
          "EPOLLRDBAND", Int;
          "EPOLLWRNORM", Int;
          "EPOLLWRBAND", Int;
          "EPOLLMSG", Int;
          "EPOLLERR", Int;
          "EPOLLHUP", Int;
          "EPOLLRDHUP", Int;
          "EPOLLEXCLUSIVE", Int;
          "EPOLLWAKEUP", Int;
          "EPOLLONESHOT", Int;
          "EPOLLET", Int;
          "EPOLL_CTL_ADD", Int;
          "EPOLL_CTL_DEL", Int;
          "EPOLL_CTL_MOD", Int;
          "sizeof(struct epoll_event)", Int;
        ]
  in
  let defs = List.map (function
      | name, C.C_define.Value.Int v ->
        let name =
          match name with
          | "sizeof(struct pollfd)" -> "sizeof_pollfd"
          | "sizeof(struct epoll_event)" -> "sizeof_epoll_event"
          | nm -> nm
        in
        Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
      | _ -> assert false
    ) (poll_defs @ epoll_defs)
  in
  C.Flags.write_lines "config.ml" (defs @ has_list)
