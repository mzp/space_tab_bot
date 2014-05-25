type size_t = int

structure Curl = struct
  type t = unit ptr

  datatype code
    = CURLE_OK
    | CURLE_Error of int

  datatype globalOption
    = CURL_GLOBAL_SSL
    | CURL_GLOBAL_WIN32
    | CURL_GLOBAL_ALL
    | CURL_GLOBAL_NOTHING
    | CURL_GLOBAL_DEFAULT

  datatype curlOption
    = CURLOPT_URL of string
    | CURLOPT_PORT of int
    | CURLOPT_USERPWD of string
    | CURLOPT_WRITEFUNCTION of word8 vector -> int
    | CURLOPT_POSTFIELDS of string
    | CURLOPT_USERAGENT of string
    | CURLOPT_POST
    | CURLOPT_POSTFIELDSIZE of int

  fun toCurlCode 0 = CURLE_OK
    | toCurlCode n = CURLE_Error n

  fun ofCurlGlobalOpt opts =
      let
        open Word
        infix <<
        infix orb
        fun ofCurlGlobalOpt1 CURL_GLOBAL_NOTHING =
            0w0
          | ofCurlGlobalOpt1 CURL_GLOBAL_SSL =
            0w1 << 0w0
          | ofCurlGlobalOpt1 CURL_GLOBAL_WIN32 =
            0w1 << 0w1
          | ofCurlGlobalOpt1 CURL_GLOBAL_DEFAULT =
            ofCurlGlobalOpt1 CURL_GLOBAL_ALL
          | ofCurlGlobalOpt1 CURL_GLOBAL_ALL =
            ofCurlGlobalOpt [CURL_GLOBAL_SSL, CURL_GLOBAL_WIN32]
      in
        List.foldl (fn (x, r) => ofCurlGlobalOpt1 x orb r) 0w0 opts
      end

  fun global_init opts =
      toCurlCode
        (_ffiapply _import "curl_global_init"
                   (ofCurlGlobalOpt opts : word) : int)

  val easy_init =
      _import "curl_easy_init" : () -> t

  fun easy_perform curl =
      toCurlCode
        (_ffiapply _import "curl_easy_perform"
                   (curl : t) : int)

  val easy_cleanup =
      _import "curl_easy_cleanup" : t -> ()

  type CURLoption = int

  local
    val curl_easy_set_string_opt =
        _import "curl_easy_setopt" : (t, CURLoption, string) -> int
    val curl_easy_set_int_opt =
        _import "curl_easy_setopt" : (t, CURLoption, int) -> int
    val curl_easy_set_write_function_opt =
        _import "curl_easy_setopt"
        : (t,
           CURLoption,
           (word8 ptr, size_t, size_t, unit ptr) -> size_t)
          -> int
    val LONG = 0
    val OBJECTPOINT = 10000
    val FUNCTIONPOINT = 20000
    val OFF_T = 20000
    fun optCode0 (CURLOPT_URL _) = (OBJECTPOINT, 2)
      | optCode0 (CURLOPT_PORT _) = (LONG, 3)
      | optCode0 (CURLOPT_USERPWD _) = (OBJECTPOINT, 5)
      | optCode0 (CURLOPT_WRITEFUNCTION _) = (FUNCTIONPOINT, 11)
      | optCode0 (CURLOPT_POSTFIELDS _) = (OBJECTPOINT, 15)
      | optCode0 (CURLOPT_USERAGENT _) = (OBJECTPOINT, 18)
      | optCode0 (CURLOPT_POST) = (LONG, 47)
      | optCode0 (CURLOPT_POSTFIELDSIZE _) = (LONG,60)
    fun optCode opt = op + (optCode0 opt)
  in
    fun easy_setopt curl (opt as CURLOPT_URL s) =
        toCurlCode (curl_easy_set_string_opt (curl, optCode opt, s))
      | easy_setopt curl (opt as CURLOPT_PORT n) =
        toCurlCode (curl_easy_set_int_opt (curl, optCode opt, n))
      | easy_setopt curl (opt as CURLOPT_USERPWD s) =
        toCurlCode (curl_easy_set_string_opt (curl, optCode opt, s))
      | easy_setopt curl (opt as CURLOPT_WRITEFUNCTION f) =
        let
          fun g (ptr, size, nmemb, _) =
              (f o Pointer.importBytes) (ptr, size * nmemb)
        in
          toCurlCode (curl_easy_set_write_function_opt (curl, optCode opt, g))
        end
      | easy_setopt curl (opt as CURLOPT_POSTFIELDS str) =
        toCurlCode (curl_easy_set_string_opt (curl, optCode opt, str))
      | easy_setopt curl (opt as CURLOPT_USERAGENT str) =
        toCurlCode (curl_easy_set_string_opt (curl, optCode opt, str))
      | easy_setopt curl (opt as CURLOPT_POST) =
        toCurlCode (curl_easy_set_int_opt (curl, optCode opt, 1))
      | easy_setopt curl (opt as CURLOPT_POSTFIELDSIZE n) =
        toCurlCode (curl_easy_set_int_opt (curl, optCode opt, n))
  end
end

