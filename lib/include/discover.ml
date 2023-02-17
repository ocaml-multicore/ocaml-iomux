module C = Configurator.V1

let () =
  C.main ~name:"discover" @@ fun c ->
  let defs =
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
    |> List.map (function
        | name, C.C_define.Value.Int v ->
          let name = 
            match name with
            | "sizeof(struct pollfd)" -> "sizeof_pollfd"
            | nm -> nm 
          in
          Printf.sprintf "let %s = 0x%x" (String.lowercase_ascii name) v
        | _ -> assert false
      )
  in
  C.Flags.write_lines "config.ml" defs
