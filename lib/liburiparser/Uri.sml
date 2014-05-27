structure Uri =
struct
  type t =
       {                (* EXAMPLE     url *)
         scheme : string,           (* http *)
         userInfo : string option,  (* user:pass *)
         host : string,             (* hoge.com *)
         port : int option,         (* 8080 *)
         path : string option,      (* hoge/fuga/index.html *)
         query : string option,     (* buyo=piyo *)
         frag : string option       (* buyo *)
       }

  val uriParser = _import "uriparser" : string -> int
  val URI_SUCCESS = 0

  fun fromString uri =
    if uriParser uri = URI_SUCCESS
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
                   else (Int.fromString o String.substring) (uri, first, length)

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

  (* FIXME: now assuming only url not urn *)
  fun toString {scheme, userInfo, host, port, path, query, frag} =
    let
      val scheme = scheme ^ "://"
      val userInfo = case userInfo of SOME s => s ^ "@" | NONE => ""
      val host = host
      val port = case port of SOME n => ":" ^ Int.toString n ^ "/" | NONE => "/"
      val path = case path of SOME s => s | NONE => ""
      val query = case query of SOME s => "?" ^ s | NONE => ""
      val frag = case frag of SOME s => "#" ^ s | NONE => ""
    in
      scheme ^ userInfo ^ host ^ port ^ path ^ query ^ frag
    end
end
