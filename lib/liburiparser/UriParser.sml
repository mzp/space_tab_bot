structure UriParser
            : sig
              type uri =
                   {
                     scheme : string,
                     userInfo : string option,
                     host : string,
                     port : int option,
                     path : string option,
                     query : string option,
                     frag : string option
                   } option
              val parse : string -> uri
(*
              val parseUserInfo : uri -> {user: string, pass: string}
              val parsePath : uri -> string list
              val parseQuery : uri -> {hoge:hoge, fuga:fuga} list
*)
            end
=
struct
  local
    open Base
  in
  type uri =
       {
         scheme : string,
         userInfo : string option,
         host : string,
         port : int option,
         path : string option,
         query : string option,
         frag : string option
       } option

  val uriParser = _import "uriparser" : string -> int

  fun parse uri =
      if uriParser uri = 0
      then
        let
          val first = (_import "get_scheme_first" : () -> int) ()
          val length = (_import "get_scheme_length" : () -> int) ()
          val scheme = String.substring (uri, first, length)

          val first = (_import "get_userInfo_first" : () -> int) ()
          val length = (_import "get_userInfo_length" : () -> int) ()
          val userInfo = if length = 0 then NONE
                         else SOME (String.substring (uri, first, length))

          val first = (_import "get_host_first" : () -> int) ()
          val length = (_import "get_host_length" : () -> int) ()
          val host = String.substring (uri, first, length)

          val first = (_import "get_port_first" : () -> int) ()
          val length = (_import "get_port_length" : () -> int) ()
          val port = if length = 0 then NONE
                     else Int.fromString $ String.substring (uri, first, length)

          val first = (_import "get_path_first" : () -> int) ()
          val length = (_import "get_path_length" : () -> int) ()
          val path = if length = 0 then NONE
                     else SOME (String.substring (uri, first, length))

          val first = (_import "get_query_first" : () -> int) ()
          val length = (_import "get_query_length" : () -> int) ()
          val query = if length = 0 then NONE
                      else SOME (String.substring (uri, first, length))

          val first = (_import "get_frag_first" : () -> int) ()
          val length = (_import "get_frag_length" : () -> int) ()
          val frag = if length = 0 then NONE
                     else SOME (String.substring (uri, first, length))
        in
          SOME {
            scheme = scheme,
            userInfo = userInfo,
            host = host,
            port = port,
            path = path,
            query = query,
            frag = frag
          }
        end
      else NONE
  end
end
