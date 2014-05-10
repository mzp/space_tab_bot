structure Jansson = struct
  type t = unit ptr

  type size_t = int
  type flag_t = word
  type json_iter = unit ptr
  type json_error_t = unit ptr

  (** higher level interface *)
  datatype json_type
    = Object
    | Array
    | String
    | Integer
    | Real
    | True
    | False
    | Null

  val types = [Object, Array, String, Integer, Real, True, False, Null]

  datatype encodingOption
    = JSON_INDENT of int
    | JSON_COMPACT
    | JSON_ENSURE_ASCII
    | JSON_SORT_KEYS
    | JSON_PRESERVE_ORDER
    | JSON_ENCODE_ANY
    | JSON_ESCAPE_SLASH
    | JSON_REAL_PRECISION of int

  datatype decodingOption
    = JSON_REJECT_DUPLICATES
    | JSON_DISABLE_EOF_CHECK
    | JSON_DECODE_ANY
    | JSON_DECODE_INT_AS_REAL
    | JSON_ALLOW_NUL

  type errorInfo =
       { line     : int
       , column   : int
       , position : int
       , source   : string
       , text     : string
       }

  exception encodingError of errorInfo

  exception decodingError of errorInfo

  exception janssonError of string

  (* utils *)
  (* wrappers. `stb_' prefixed functions are defined in janssonext.c *)
  val error_t_handle =
      _import "stb_json_error_t" : () -> json_error_t

  val currentErrorLine =
      _import "stb_json_error_t_line" : () -> int

  val currentErrorColumn =
      _import "stb_json_error_t_column" : () -> int

  val currentErrorPosition =
      _import "stb_json_error_t_position" : () -> int

  fun currentErrorSource () =
      Pointer.importString
        (_ffiapply _import "stb_json_error_t_source" () : char ptr)

  fun currentErrorText () =
      Pointer.importString
        (_ffiapply _import "stb_json_error_t_text" () : char ptr)

  fun currentError () =
      { line     = currentErrorLine ()
      , column   = currentErrorColumn ()
      , position = currentErrorPosition ()
      , source   = currentErrorSource ()
      , text     = currentErrorText ()
      }

  fun opt v =
      if Pointer.isNull v then
        NONE
      else
        SOME v

  fun exn e v =
      if Pointer.isNull v then
        raise e
      else
        v

  fun tryJansson s n =
      if n = ~1 then
        raise (janssonError s)
      else
        ()

  fun tryEncDec err n =
      if Pointer.isNull n then
        raise (err (currentError ()))
      else
        n

  fun findIndex x ys =
      let
        fun loop _ [] = NONE
          | loop i (y::ys) =
            if x = y then
              SOME i
            else
              loop (i + 1) ys
      in
        loop 0 ys
      end

  fun intOfType typ = Option.valOf (findIndex typ types)

  fun typeOfInt n = List.nth (types, n)

  fun wordOfEncodingOptions opts =
      let
        open Word
        infix andb
        infix orb
        infix <<
        fun woe (JSON_INDENT n) =
            Word.fromInt n andb 0wx1f
          | woe JSON_COMPACT = 0wx20
          | woe JSON_ENSURE_ASCII = 0wx40
          | woe JSON_SORT_KEYS = 0wx80
          | woe JSON_PRESERVE_ORDER = 0wx100
          | woe JSON_ENCODE_ANY = 0wx200
          | woe JSON_ESCAPE_SLASH = 0wx400
          | woe (JSON_REAL_PRECISION n) =
            (Word.fromInt n andb 0wx1f) << 0w11
      in
        List.foldl (fn (opt, r) => woe opt orb r) 0w0 opts
      end

  fun wordOfDecodingOptions opts =
      let
        open Word
        infix orb
        fun wod JSON_REJECT_DUPLICATES = 0wx1
          | wod JSON_DISABLE_EOF_CHECK = 0wx2
          | wod JSON_DECODE_ANY = 0wx4
          | wod JSON_DECODE_INT_AS_REAL = 0wx8
          | wod JSON_ALLOW_NUL = 0wx10
      in
        List.foldl (fn (opt, r) => wod opt orb r) 0w0 opts
      end

  fun typeof v =
      typeOfInt
        (_ffiapply
           _import "stb_json_typeof" (v : t) : int)

  (* constructors *)
  val object =
      _import "json_object" : () -> t

  val array =
      _import "json_array" : () -> t

  val string =
      _import "json_string" : string -> t

  val integer =
      _import "stb_json_integer" : int -> t

  val real =
      _import "json_real" : real -> t

  val true_ =
      _import "json_true" : () -> t

  val false_ =
      _import "json_false" : () -> t

  fun boolean b =
      if b then true_ () else false_ ()

  val null =
      _import "json_null" : () -> t

  (* reference count *)
  val incref =
      _import "stb_json_incref" : t -> t
  val decref =
      _import "stb_json_decref" : t -> ()

  (* objects *)
  val object_size =
      _import "json_object_size" : t -> size_t

  fun object_get (obj, key) =
      opt
        (_ffiapply
           _import "json_object_get"
           ( obj : t
           , key : string) : t)

  fun object_set (obj, key, value) =
      tryJansson
        "object_set"
        (_ffiapply
           _import "stb_json_object_set"
           ( obj : t
           , key : string
           , value : t) : int)

  fun object_set_new (obj, key, value) =
      tryJansson
        "object_set_new"
        (_ffiapply
           _import "json_object_set_new"
           ( obj : t
           , key : string
           , value : t) : int)

  fun object_del (obj, key) =
      tryJansson
        "object_del"
        (_ffiapply
           _import "json_object_del"
           ( obj : t
           , key : string) : int)

  fun object_clear obj =
      tryJansson
        "object_clear"
        (_ffiapply
           _import "json_object_clear"
           (obj : t) : int)

  fun object_update (obj, otherObj) =
      tryJansson
        "object_update"
        (_ffiapply
           _import "json_object_update"
           ( obj : t
           , otherObj : t) : int)

  fun object_update_existing (obj, otherObj) =
      tryJansson
        "object_update_existing"
        (_ffiapply
           _import "json_object_update_existing"
           ( obj : t
           , otherObj : t) : int)

  fun object_update_missing (obj, otherObj) =
      tryJansson
        "object_update_missing"
        (_ffiapply
           _import "json_object_update_missing"
           ( obj : t
           , otherObj : t) : int)

  fun object_foreach f obj =
      _ffiapply
        _import "stb_json_object_foreach"
        ( obj : t
        , (fn (pk, v) => f (Pointer.importString pk, v)) : (char ptr, t) -> ()
        ) : ()

  (* arrays *)
  val array_size =
      _import "json_array_size" : t -> size_t

  fun array_get (arr, i) =
      exn
        Subscript
        (_ffiapply
           _import "json_array_get"
           ( arr : t
           , i : size_t) : t)

  fun array_set (arr, i, value) =
      tryJansson
        "array_set"
        (_ffiapply
           _import "stb_json_array_set"
           ( arr : t
           , i : size_t
           , value : t) : int)

  fun array_set_new (arr, i, value) =
      tryJansson
        "array_set_new"
        (_ffiapply
           _import "json_array_set_new"
           ( arr : t
           , i : size_t
           , value : t) : int)

  fun array_append (arr, value) =
      tryJansson
        "array_append"
        (_ffiapply
           _import "stb_json_array_append"
           ( arr : t
           , value : t) : int)

  fun array_append_new (arr, value) =
      tryJansson
        "array_append_new"
        (_ffiapply
           _import "json_array_append_new"
           ( arr : t
           , value : t) : int)

  fun array_insert (arr, i, value) =
      tryJansson
        "array_insert"
        (_ffiapply
           _import "stb_json_array_insert"
           ( arr : t
           , i : size_t
           , value : t) : int)

  fun array_insert_new (arr, i, value) =
      tryJansson
        "array_insert_new"
        (_ffiapply
           _import "json_array_insert_new"
           ( arr : t
           , i : size_t
           , value : t) : int)

  fun array_remove (arr, i) =
      tryJansson
        "array_remove"
        (_ffiapply
           _import "json_array_remove"
           ( arr : t
           , i : size_t) : int)

  fun array_clear arr =
      tryJansson
        "array_clear"
        (_ffiapply
           _import "json_array_clear"
           (arr : t) : int)

  fun array_extend (arr, otherArray) =
      tryJansson
        "array_extend"
        (_ffiapply
           _import "json_array_extend"
           ( arr : t
           , otherArray : t) : int)

  fun array_foreachi f arr =
      _ffiapply
        _import "stb_json_array_foreachi"
        ( arr : t
        , f : (int, t) -> ()
        ) : ()

  fun array_foreach f arr =
      array_foreachi (fn (_, v) => f v) arr

  (* strings *)
  fun string_value tstr =
      Pointer.importString
        (_ffiapply
           _import "json_string_value" (tstr : t) : char ptr)

  fun string_set (json, value) =
      tryJansson
        "string_set"
        (_ffiapply
           _import "json_string_set"
           ( json : t
           , value : string ) : int)

  (* integers *)
  val integer_value =
      _import "stb_json_integer_value" : t -> int

  fun integer_set (json, value) =
      tryJansson
        "integer_set"
        (_ffiapply
           _import "json_integer_set"
           ( json : t
           , value : int ) : int)

  (* reals *)
  val real_value =
      _import "json_real_value" : t -> real

  fun real_set (json, value) =
      tryJansson
        "real_set"
        (_ffiapply
           _import "json_real_set"
           ( json : t
           , value : real ) : int)

  val number_value =
      _import "json_number_value" : t -> real

  (* equality *)
  fun equal (a, b) =
      0 <> (_ffiapply
             _import "json_equal" (a : t, b : t) : int)

  (* copying *)
  val copy =
      _import "json_copy" : t -> t

  val deep_copy =
      _import "json_deep_copy" : t -> t

  (* decoding *)
  fun loads input opts =
      tryEncDec
        decodingError
        (_ffiapply
           _import "json_loads"
           (input : string,
            wordOfDecodingOptions opts : flag_t,
            error_t_handle () : json_error_t
           ) : t)

  fun load_file path opts =
      tryEncDec
        decodingError
        (_ffiapply
           _import "json_load_file"
           (path : string,
            wordOfDecodingOptions opts : flag_t,
            error_t_handle () : json_error_t
           ) : t)

  (* encoding *)
  fun dumps json opts =
      Pointer.importString
        (tryEncDec
           encodingError
           (_ffiapply
              _import "json_load_file"
              (json : t,
               wordOfEncodingOptions opts : flag_t,
               error_t_handle () : json_error_t
              ) : char ptr))
end
