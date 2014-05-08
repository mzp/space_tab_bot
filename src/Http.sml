structure Http = struct
  type httpOptions =
       { username : string option
       , password : string option
       }

  val emptyOptions : httpOptions =
      { username = NONE
      , password = NONE
      }

  fun httpGet opts url =
      let
        open Base
        val rs = ref []
        fun write w8s =
            ( rs := w8s :: !rs
            ; Vector.length w8s )
        (* FIXME: error handling *)
        val _ = Curl.global_init [Curl.CURL_GLOBAL_DEFAULT]
        val curl = Curl.easy_init ()
        val _ = Curl.easy_setopt curl (Curl.CURLOPT_URL url)
        val _ = Curl.easy_setopt curl (Curl.CURLOPT_WRITEFUNCTION write)
        val _ = case (#username opts, #password opts) of
                    (SOME u, SOME p) =>
                    ( Curl.easy_setopt curl (Curl.CURLOPT_USERPWD $ u ^ ":" ^ p)
                    ; ())
                  | _ => ()
      in
        protectx curl
                 (fn curl =>
                     ( Curl.easy_perform curl
                     ; !rs |> List.rev |> Vector.concat
                 ))
                 Curl.easy_cleanup
      end
end
