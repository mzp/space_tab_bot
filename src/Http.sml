structure Http = struct
  open Base

  type httpOptions =
       { username : string option
       , password : string option
       }

  val emptyOptions : httpOptions =
      { username = NONE
      , password = NONE
      }

  val userAgent = "SpaceTabBot/1.0.0 (see: http://github.com/mzp/space_tab_bot)"

  fun perform setopt =
      let
        val rs = ref []
        fun write w8s =
            ( rs := w8s :: !rs
            ; Vector.length w8s )
        (* FIXME: error handling *)
        val _ = Curl.global_init [Curl.CURL_GLOBAL_DEFAULT]
        val curl = Curl.easy_init ()
        val _ = Curl.easy_setopt curl (Curl.CURLOPT_WRITEFUNCTION write)
        val _ = Curl.easy_setopt curl (Curl.CURLOPT_USERAGENT userAgent)
        val ()   = setopt curl
      in
        protectx curl
                 (fn curl =>
                     ( Curl.easy_perform curl
                     ; !rs |> List.rev |> Vector.concat
                 ))
                 Curl.easy_cleanup
      end

  fun httpGet opts url =
      perform (fn curl =>
        let
          val _ = Curl.easy_setopt curl (Curl.CURLOPT_URL url)
          val _ = case (#username opts, #password opts) of
                    (SOME u, SOME p) =>
                    ( Curl.easy_setopt curl (Curl.CURLOPT_USERPWD $ u ^ ":" ^ p)
                    ; ())
                  | _ => ()
        in
          ()
        end)

  fun httpPost opts url fields =
      perform (fn curl =>
        let
          val _ = Curl.easy_setopt curl (Curl.CURLOPT_URL url)
          val _ = Curl.easy_setopt curl  Curl.CURLOPT_POST
          val _ = Curl.easy_setopt curl (Curl.CURLOPT_POSTFIELDSIZE (String.size fields))
          val _ = Curl.easy_setopt curl (Curl.CURLOPT_POSTFIELDS fields)
          val _ = case (#username opts, #password opts) of
                    (SOME u, SOME p) =>
                    ( Curl.easy_setopt curl (Curl.CURLOPT_USERPWD $ u ^ ":" ^ p)
                    ; ())
                  | _ => ()
        in
          ()
        end)
end
