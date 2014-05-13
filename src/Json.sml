structure Json = struct
  open Base

  datatype t
    = True
    | False
    | Null
    | String of string
    | Integer of int
    | Real of real
    | Object of (string * t) list
    | Array of t list

  fun push r v =
      r := v :: !r

  fun fromJansson x =
      case Jansson.typeof x of
          Jansson.True => True
        | Jansson.False => False
        | Jansson.Null => Null
        | Jansson.String =>
          String $ Jansson.string_value x
        | Jansson.Integer =>
          Integer $ Jansson.integer_value x
        | Jansson.Real =>
          Real $ Jansson.real_value x
        | Jansson.Object =>
          let
            val rs = ref []
          in
            x |> Jansson.object_foreach
              (fn (k, v) => push rs (k, fromJansson v))
          ; Object $ List.rev (!rs)
          end
        | Jansson.Array =>
          let
            val rs = ref []
          in
            x |> Jansson.array_foreach (push rs o fromJansson)
          ; Array $ List.rev (!rs)
          end

  fun parseString s =
      (* FIXME: don't expose Jansson.decodingError *)
      protectx
        (Jansson.loads s [])
        fromJansson
        Jansson.decref

  fun parseFile path =
      (* FIXME: don't expose Jansson.decodingError *)
      protectx
        (Jansson.load_file path [Jansson.JSON_DECODE_ANY])
        fromJansson
        Jansson.decref

  fun toJansson True = Jansson.true_ ()
    | toJansson False = Jansson.false_ ()
    | toJansson Null = Jansson.null ()
    | toJansson (String s) = Jansson.string s
    | toJansson (Integer i) = Jansson.integer i
    | toJansson (Real n) = Jansson.real n
    | toJansson (Object kvs) =
      let
        val obj = Jansson.object ()
      in
        List.app (fn (k, v) => Jansson.object_set_new (obj, k, toJansson v)) kvs
      ; obj
      end
    | toJansson (Array xs) =
      let
        val arr = Jansson.array ()
      in
        List.app (fn x => Jansson.array_append_new (arr, toJansson x)) xs
      ; arr
      end

  fun toString v =
      (* FIXME: don't expose Jansson.encodingError *)
      protectx (toJansson v)
               (flip Jansson.dumps [])
               Jansson.decref
end
