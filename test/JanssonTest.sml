(* -*- coding: utf-8 -*- *)
structure JanssonTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert

  open Base

  local
    open Jansson
  in
    fun stringOf_json_type Object = "Object"
      | stringOf_json_type Array = "Array"
      | stringOf_json_type String = "String"
      | stringOf_json_type Integer = "Integer"
      | stringOf_json_type Real = "Real"
      | stringOf_json_type True = "True"
      | stringOf_json_type False = "False"
      | stringOf_json_type Null = "Null"
  end

  fun ephemeral v body = protectx v body Jansson.decref

  fun ephemeral2 v body =
      protectx v body
               (fn (a, b) =>
                   ( Jansson.decref a
                   ; Jansson.decref b
               ))

  fun ephemeral3 v body =
      protectx v body
               (fn (a, b, c) =>
                   ( Jansson.decref a
                   ; Jansson.decref b
                   ; Jansson.decref c
               ))

  fun ephemeral4 v body =
      protectx v body
               (fn (a, b, c, d) =>
                   ( Jansson.decref a
                   ; Jansson.decref b
                   ; Jansson.decref c
                   ; Jansson.decref d
               ))

  fun ephemeral5 v body =
      protectx v body
               (fn (a, b, c, d, e) =>
                   ( Jansson.decref a
                   ; Jansson.decref b
                   ; Jansson.decref c
                   ; Jansson.decref d
                   ; Jansson.decref e
               ))

  fun hasType typ v = (Jansson.typeof v) = typ

  fun assertHasType typ v =
      ephemeral
        v
        (fn x =>
            Assert.assertEqual
              (op =) stringOf_json_type typ (Jansson.typeof x))

  fun assertFailWithExceptionEquallyNamed exn f =
      ( f ()
      ; Assert.fail ("not raise " ^ exnName exn))
      handle e => Assert.assertEqualExceptionName exn e

  fun assertJanssonEqual x y =
      Assert.assertEqual Jansson.equal (fn x => Jansson.dumps x []) x y

  fun object_test () =
      assertHasType Jansson.Object (Jansson.object ())

  fun array_test () =
      assertHasType Jansson.Array (Jansson.array ())

  fun string_test () =
      assertHasType Jansson.String (Jansson.string "Quod erat demonstrundum")

  fun integer_test () =
      assertHasType Jansson.Integer (Jansson.integer 42)

  fun real_test () =
      assertHasType Jansson.Real (Jansson.real 3.14)

  fun true_test () =
      assertHasType Jansson.True (Jansson.true_ ())

  fun false_test () =
      assertHasType Jansson.False (Jansson.false_ ())

  fun boolean_test () =
      ( assertHasType Jansson.True (Jansson.boolean true)
      ; assertHasType Jansson.False (Jansson.boolean false)
      )

  fun null_test () =
      assertHasType Jansson.Null (Jansson.null ())

  fun value_test assert cons get v =
      ephemeral
        (cons v)
        (fn x => assert v (get x))

  fun value_set_test assert cons (get, set) (v, new) =
      ephemeral
        (cons v)
        (fn x =>
            ( set(x, new)
            ; assert new (get x)))

  fun string_value_test () =
      value_test
        Assert.assertEqualString Jansson.string Jansson.string_value "foobar"

  fun string_set_test () =
      value_set_test
        Assert.assertEqualString
        Jansson.string
        (Jansson.string_value, Jansson.string_set)
        ("foobar", "quux")

  fun integer_value_test () =
      value_test
        Assert.assertEqualInt Jansson.integer Jansson.integer_value 57

  fun integer_set_test () =
      value_set_test
        Assert.assertEqualInt
        Jansson.integer
        (Jansson.integer_value, Jansson.integer_set)
        (57, 59)

  fun real_value_test () =
      value_test
        Assert.assertEqualReal Jansson.real Jansson.real_value 2.236

  fun real_set_test () =
      value_set_test
        Assert.assertEqualReal
        Jansson.real
        (Jansson.real_value, Jansson.real_set)
        (2.236, 2.2362)

  fun number_value_test () =
      List.app
        (fn (x, v) =>
            ephemeral
              x
              (fn x =>
                  Assert.assertEqualReal v (Jansson.number_value x)))
        [ (Jansson.object (), 0.0)
        , (Jansson.array (), 0.0)
        , (Jansson.string "1", 0.0)
        , (Jansson.integer 128, 128.0)
        , let val v = 2.718 in (Jansson.real v, v) end
        , (Jansson.true_ (), 0.0)
        , (Jansson.false_ (), 0.0)
        , (Jansson.null (), 0.0)
        ]

  fun equal_test () =
      List.app
        (fn (x, y, b) =>
            ephemeral2
              (x, y)
              (fn (x, y) =>
                  Assert.assertEqualBool b (Jansson.equal (x, y))))
        [ (Jansson.object (), Jansson.object (), true)
        , (Jansson.array (), Jansson.array (), true)
        , (Jansson.object (), Jansson.array (), false)
        , (Jansson.string "", Jansson.string "", true)
        , (Jansson.string "a", Jansson.string "b", false)
        , (Jansson.integer 0, Jansson.integer 0, true)
        , (Jansson.integer 0, Jansson.integer 1, false)
        , (Jansson.real 0.0, Jansson.real 0.0, true)
        , (Jansson.real 0.0, Jansson.real 2.0, false)
        , (Jansson.true_ (), Jansson.true_ (), true)
        , (Jansson.true_ (), Jansson.boolean true, true)
        , (Jansson.false_ (), Jansson.boolean false, true)
        , (Jansson.true_ (), Jansson.false_ (), false)
        ]

  fun object_size_test () =
      ephemeral
        (Jansson.object ())
        (fn x => Assert.assertEqualInt 0 (Jansson.object_size x))

  fun object_set_test () =
      ephemeral3
        (Jansson.object (), Jansson.object (), Jansson.object ())
        (fn (x, y, z) =>
            let
              val n = Jansson.object_size x
              val key = "k"
            in
              Jansson.object_set (x, key, y)
            ; Assert.assertEqualInt (n + 1) (Jansson.object_size x)
            ; Jansson.object_set (x, key, z)
            ; Assert.assertEqualInt (n + 1) (Jansson.object_size x)
            end)

  fun object_set_new_test () =
      ephemeral
        (Jansson.object ())
        (fn x =>
            let
              val n = Jansson.object_size x
              val y = Jansson.object ()
              val z = Jansson.object ()
              val key = "k"
            in
              Jansson.object_set_new (x, key, y)
            ; Assert.assertEqualInt (n + 1) (Jansson.object_size x)
            ; Jansson.object_set_new (x, key, z)
            ; Assert.assertEqualInt (n + 1) (Jansson.object_size x)
            end)

  fun object_get_test () =
      ephemeral
        (Jansson.object ())
        (fn x =>
            ephemeral
              (Jansson.string "foo")
              (fn y =>
                  let
                    val key = "k"
                    val key2 = "kk"
                    val () = Jansson.object_set (x, key, y)
                    val v = Jansson.object_get (x, key)
                  in
                    Assert.assertSome v
                  ; assertJanssonEqual y (Option.valOf v)
                  ; Assert.assertNone (Jansson.object_get (x, key2))
                  end))

  fun object_del_test () =
      ephemeral
        (Jansson.object ())
        (fn x =>
            let
              val key = "k"
              val key2 = "kk"
              val () = Jansson.object_set (x, key, Jansson.object ())
              val n = Jansson.object_size x
            in
              Jansson.object_del (x, key)
            ; Assert.assertEqualInt (n - 1) (Jansson.object_size x)
            ; assertFailWithExceptionEquallyNamed
                (Jansson.janssonError "")
                (fn () => Jansson.object_del (x, key))
            ; assertFailWithExceptionEquallyNamed
                (Jansson.janssonError "")
                (fn () => Jansson.object_del (x, key2))
            end)

  fun object_clear_test () =
      ephemeral
        (Jansson.object ())
        (fn x =>
            let
              val () = Jansson.object_set_new (x, "k", Jansson.integer 0)
              val () = Jansson.object_set_new (x, "kk", Jansson.integer 1)
            in
              Assert.assertEqualInt 2 (Jansson.object_size x)
            ; Jansson.object_clear x
            ; Assert.assertEqualInt 0 (Jansson.object_size x)
            ; assertFailWithExceptionEquallyNamed
                (Jansson.janssonError "")
                (fn () =>
                    ephemeral (Jansson.integer 0) Jansson.object_clear)
            end)

  fun object_update_test () =
      ephemeral5
        ( Jansson.object ()
        , Jansson.object ()
        , Jansson.string "foo"
        , Jansson.string "bar"
        , Jansson.string "baz"
        )
        (fn (x, y, v1, v2, v3) =>
            let
              val k1 = "a"
              val k2 = "b"
              val k3 = "c"
              val () = Jansson.object_set (x, k1, v1)
              val () = Jansson.object_set (x, k2, v2)
              val n = Jansson.object_size x
              val () = Jansson.object_set (y, k2, v3)
              val () = Jansson.object_set (y, k3, v1)
              val () = Jansson.object_update (x, y)
              val () = Assert.assertEqualInt 3 (Jansson.object_size x)
              val ov1 = Jansson.object_get (x, k1)
              val ov2 = Jansson.object_get (x, k2)
              val ov3 = Jansson.object_get (x, k3)
            in
              Assert.assertSome ov1
            ; Assert.assertSome ov2
            ; Assert.assertSome ov3
            ; assertJanssonEqual v1 (Option.valOf ov1)
            ; assertJanssonEqual v3 (Option.valOf ov2)
            ; assertJanssonEqual v1 (Option.valOf ov3)
            end)

  fun object_update_existing_test () =
      ephemeral5
        ( Jansson.object ()
        , Jansson.object ()
        , Jansson.string "foo"
        , Jansson.string "bar"
        , Jansson.string "baz"
        )
        (fn (x, y, v1, v2, v3) =>
            let
              val k1 = "a"
              val k2 = "b"
              val k3 = "c"
              val () = Jansson.object_set (x, k1, v1)
              val () = Jansson.object_set (x, k2, v2)
              val n = Jansson.object_size x
              val () = Jansson.object_set (y, k2, v3)
              val () = Jansson.object_set (y, k3, v1)
              val () = Jansson.object_update_existing (x, y)
              val () = Assert.assertEqualInt n (Jansson.object_size x)
              val ov1 = Jansson.object_get (x, k1)
              val ov2 = Jansson.object_get (x, k2)
              val ov3 = Jansson.object_get (x, k3)
            in
              Assert.assertSome ov1
            ; Assert.assertSome ov2
            ; Assert.assertNone ov3
            ; assertJanssonEqual v1 (Option.valOf ov1)
            ; assertJanssonEqual v3 (Option.valOf ov2)
            end)

  fun object_update_missing_test () =
      ephemeral5
        ( Jansson.object ()
        , Jansson.object ()
        , Jansson.string "foo"
        , Jansson.string "bar"
        , Jansson.string "baz"
        )
        (fn (x, y, v1, v2, v3) =>
            let
              val k1 = "a"
              val k2 = "b"
              val k3 = "c"
              val () = Jansson.object_set (x, k1, v1)
              val () = Jansson.object_set (x, k2, v2)
              val n = Jansson.object_size x
              val () = Jansson.object_set (y, k2, v3)
              val () = Jansson.object_set (y, k3, v1)
              val () = Jansson.object_update_missing (x, y)
              val () = Assert.assertEqualInt 3 (Jansson.object_size x)
              val ov1 = Jansson.object_get (x, k1)
              val ov2 = Jansson.object_get (x, k2)
              val ov3 = Jansson.object_get (x, k3)
            in
              Assert.assertSome ov1
            ; Assert.assertSome ov2
            ; Assert.assertSome ov3
            ; assertJanssonEqual v1 (Option.valOf ov1)
            ; assertJanssonEqual v2 (Option.valOf ov2)
            ; assertJanssonEqual v1 (Option.valOf ov3)
            end)

  fun object_foreach_test () =
      ephemeral5
        ( Jansson.object ()
        , Jansson.string ""
        , Jansson.null ()
        , Jansson.true_ ()
        , Jansson.false_ ())
        (fn (obj, v1, v2, v3, v4) =>
            let
              val k1 = "k1"
              val k2 = "k2"
              val k3 = "k3"
              val k4 = "k4"
              val r = ref []
            in
              Jansson.object_set (obj, k1, v1)
            ; Jansson.object_set (obj, k2, v2)
            ; Jansson.object_set (obj, k3, v3)
            ; Jansson.object_set (obj, k4, v4)
            ; Jansson.object_foreach
                (fn (key, v) => r := (key, v) :: !r)
                obj
            ; List.app
                (fn (key, v) =>
                    let
                      val ov = Jansson.object_get (obj, key)
                    in
                      Assert.assertSome ov
                    ; assertJanssonEqual v (Option.valOf ov)
                    end)
                (!r)
            end)

  fun array_size_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            Assert.assertEqualInt 0 (Jansson.array_size x))

  fun array_append_test () =
      ephemeral2
        (Jansson.array (), Jansson.null ())
        (fn (x, v) =>
            let
              val n = Jansson.array_size x
              val () = Jansson.array_append (x, v)
            in
              Assert.assertEqualInt (n + 1) (Jansson.array_size x)
            end)

  fun array_append_new_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            let
              val n = Jansson.array_size x
              val () = Jansson.array_append_new (x, Jansson.null ())
            in
              Assert.assertEqualInt (n + 1) (Jansson.array_size x)
            end)

  fun array_get_test () =
      ephemeral2
        (Jansson.array (), Jansson.null ())
        (fn (x, v) =>
            let in
              assertFailWithExceptionEquallyNamed
                Subscript
                (fn () => Jansson.array_get (x, 0))
            ; Jansson.array_append (x, v)
            ; assertJanssonEqual v (Jansson.array_get (x, 0))
            end)

  fun array_set_test () =
      ephemeral3
        (Jansson.array (), Jansson.true_ (), Jansson.false_ ())
        (fn (x, v1, v2) =>
            let in
              assertFailWithExceptionEquallyNamed
                (Jansson.janssonError "")
                (fn () => Jansson.array_set (x, 0, v1))
            ; Jansson.array_append (x, v1)
            ; Jansson.array_set (x, 0, v2)
            ; assertJanssonEqual v2 (Jansson.array_get (x, 0))
            end)

  fun array_set_new_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            let
              val v1 = Jansson.true_ ()
              val v2 = Jansson.false_ ()
            in
              assertFailWithExceptionEquallyNamed
                (Jansson.janssonError "")
                (fn () => Jansson.array_set_new (x, 0, v1))
            ; Jansson.array_append_new (x, v1)
            ; Jansson.array_set_new (x, 0, v2)
            ; assertJanssonEqual v2 (Jansson.array_get (x, 0))
            end)

  fun array_insert_test () =
      ephemeral3
        (Jansson.array (), Jansson.true_ (), Jansson.false_ ())
        (fn (x, v1, v2) =>
            let
              val () = Jansson.array_append (x, v1)
              val n = Jansson.array_size x
              val () = Jansson.array_insert (x, 0, v2)
              val m = Jansson.array_size x
            in
              Assert.assertEqualInt (n + 1) m
            ; assertJanssonEqual v2 (Jansson.array_get (x, 0))
            ; assertJanssonEqual v1 (Jansson.array_get (x, 1))
            ; Jansson.array_insert (x, 1, v2)
            ; Assert.assertEqualInt (m + 1) (Jansson.array_size x)
            ; assertJanssonEqual v2 (Jansson.array_get (x, 0))
            ; assertJanssonEqual v2 (Jansson.array_get (x, 1))
            ; assertJanssonEqual v1 (Jansson.array_get (x, 2))
            end)

  fun array_insert_new_test () =
      ephemeral3
        (Jansson.array (), Jansson.true_ (), Jansson.false_ ())
        (fn (x, v1, v2) =>
            let
              val () = Jansson.array_append (x, v1)
              val n = Jansson.array_size x
              val () = Jansson.array_insert_new (x, 0, Jansson.copy v2)
              val m = Jansson.array_size x
            in
              Assert.assertEqualInt (n + 1) m
            ; assertJanssonEqual v2 (Jansson.array_get (x, 0))
            ; assertJanssonEqual v1 (Jansson.array_get (x, 1))
            ; Jansson.array_insert_new (x, 1, Jansson.copy v2)
            ; Assert.assertEqualInt (m + 1) (Jansson.array_size x)
            ; assertJanssonEqual v2 (Jansson.array_get (x, 0))
            ; assertJanssonEqual v2 (Jansson.array_get (x, 1))
            ; assertJanssonEqual v1 (Jansson.array_get (x, 2))
            end)

  fun array_remove_test () =
      ephemeral4
        (Jansson.array (), Jansson.true_ (), Jansson.false_ (), Jansson.null ())
        (fn (x, v1, v2, v3) =>
            let
              val () = assertFailWithExceptionEquallyNamed
                         (Jansson.janssonError "")
                         (fn () => Jansson.array_remove (x, 0))
              val () = Jansson.array_append (x, v1)
              val () = Jansson.array_append (x, v2)
              val () = Jansson.array_append (x, v3)
              val m = Jansson.array_size x
              val () = assertFailWithExceptionEquallyNamed
                         (Jansson.janssonError "")
                         (fn () => Jansson.array_remove (x, m))
              val () = Jansson.array_remove (x, 0)
              val () = assertJanssonEqual v2 (Jansson.array_get (x, 0))
              val () = Assert.assertEqualInt (m - 1) (Jansson.array_size x)
              val n = Jansson.array_size x
            in
              Jansson.array_remove (x, n - 1)
            ; Assert.assertEqualInt (n - 1) (Jansson.array_size x)
            ; assertJanssonEqual v2
                                 (Jansson.array_get
                                    (x, (Jansson.array_size x) - 1))
            end)

  fun array_clear_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            let
              val n = 3
              val vs = List.tabulate (n, fn _ => Jansson.null ())
            in
              List.app (fn v => Jansson.array_append_new (x, v)) vs
            ; Assert.assertEqualInt n (Jansson.array_size x)
            ; Jansson.array_clear x
            ; Assert.assertEqualInt 0 (Jansson.array_size x)
            end)

  fun array_extend_test () =
      ephemeral3
        (Jansson.array (), Jansson.array (), Jansson.array ())
        (fn (x, y, z) =>
            let
              open Base
              fun appendNulls arr n =
                  List.tabulate (n, fn _ => Jansson.null ())
                  |> List.app (fn v => Jansson.array_append_new (arr, v))
              val (n, m, l) = (3, 1, 7)
              val () = appendNulls x n
              val () = appendNulls y m
              val () = appendNulls z l
            in
              Assert.assertEqualInt n (Jansson.array_size x)
            ; Assert.assertEqualInt m (Jansson.array_size y)
            ; Assert.assertEqualInt l (Jansson.array_size z)
            ; Jansson.array_extend (x, y)
            ; Assert.assertEqualInt m (Jansson.array_size y)
            ; Assert.assertEqualInt (n + m) (Jansson.array_size x)
            ; Jansson.array_extend (x, z)
            ; Assert.assertEqualInt l (Jansson.array_size z)
            ; Assert.assertEqualInt (n + m + l) (Jansson.array_size x)
            end)

  fun array_foreach_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            let
              val arr = Array.tabulate (10, fn n => Jansson.integer n)
            in
              Array.app (fn v => Jansson.array_append_new (x, v)) arr
            ; Jansson.array_foreach
                (fn v =>
                    assertJanssonEqual v
                                       (Array.sub
                                          (arr, Jansson.integer_value v)))
                x
            end)

  fun array_foreachi_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            let
              open Base
              fun appendInts arr n =
                  List.tabulate (n, fn m => Jansson.integer m)
                  |> List.app (fn v => Jansson.array_append_new (arr, v))
              val () = appendInts x 10
              val arr = Array.array (Jansson.array_size x, Jansson.null ())
            in
              Jansson.array_foreachi
                (fn (i, v) => Array.update (arr, i, v))
                x
            ; Array.appi
                (fn (i, v) =>
                    assertJanssonEqual (Jansson.array_get (x, i)) v)
                arr
            end)

  fun loads_test () =
      ephemeral
        (Jansson.loads "[]" [])
        (fn x =>
            Jansson.array_size x
            |> Assert.assertEqualInt 0)

  val dummyErrorInfo =
       { line = 0
       , column = 0
       , position = 0
       , source = ""
       , text = ""
       }

  fun loads_dup_test () =
      let
        val str = "{\"a\": 1, \"a\": 2}"
      in
        assertFailWithExceptionEquallyNamed
          (Jansson.decodingError dummyErrorInfo)
          (fn () => Jansson.loads str [Jansson.JSON_REJECT_DUPLICATES])
      ; ephemeral (Jansson.loads str [])
                  (assertHasType Jansson.Object)
      end

  fun loads_any_test () =
      let
        val str = "null"
      in
        assertFailWithExceptionEquallyNamed
          (Jansson.decodingError dummyErrorInfo)
          (fn () => Jansson.loads str [])
      ; ephemeral (Jansson.loads str [Jansson.JSON_DECODE_ANY])
                  (assertHasType Jansson.Null)
      end

  fun loads_eof_test () =
      let
        val str = "{} null"
      in
        assertFailWithExceptionEquallyNamed
          (Jansson.decodingError dummyErrorInfo)
          (fn () => Jansson.loads str [])
      ; ephemeral (Jansson.loads str [Jansson.JSON_DISABLE_EOF_CHECK])
                  (assertHasType Jansson.Object)
      end

  fun loads_int_as_real_test () =
      let
        val str = "[1]"
      in
        ephemeral (Jansson.loads str [Jansson.JSON_DISABLE_EOF_CHECK])
                  (fn x =>
                      assertHasType Jansson.Integer (Jansson.array_get (x, 0)))
      ; ephemeral (Jansson.loads str [Jansson.JSON_DECODE_INT_AS_REAL])
                  (fn x =>
                      assertHasType Jansson.Real (Jansson.array_get (x, 0)))
      end

  fun loads_mix_test () =
      ephemeral
        (Jansson.loads "1 true" [ Jansson.JSON_DECODE_ANY
                                , Jansson.JSON_DISABLE_EOF_CHECK
                                , Jansson.JSON_DECODE_INT_AS_REAL])
        (assertHasType Jansson.Real)

  fun dumps_indent_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            ( Jansson.array_append_new (x, Jansson.integer 1)
            ; Jansson.array_append_new (x, Jansson.integer 2)
            ; Assert.assertEqualString "[1, 2]"
                                       (Jansson.dumps x [])
            ; Assert.assertEqualString "[\n  1,\n  2\n]"
                                       (Jansson.dumps x [Jansson.JSON_INDENT 2])
            ; Assert.assertEqualString "[\n   1,\n   2\n]"
                                       (Jansson.dumps x [Jansson.JSON_INDENT 3])
            ))

  fun dumps_compact_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            ( Jansson.array_append_new (x, Jansson.integer 1)
            ; Jansson.array_append_new (x, Jansson.integer 2)
            ; Assert.assertEqualString "[1, 2]"
                                       (Jansson.dumps x [])
            ; Assert.assertEqualString "[1,2]"
                                       (Jansson.dumps x [Jansson.JSON_COMPACT])
            ))

  fun dumps_any_test () =
      ephemeral
        (Jansson.null ())
        (fn x =>
            ( assertFailWithExceptionEquallyNamed
                (Jansson.encodingError dummyErrorInfo)
                (fn () => Jansson.dumps x [])
            ; Assert.assertEqualString
                "null"
                (Jansson.dumps x [Jansson.JSON_ENCODE_ANY])
        ))

  fun dumps_escape_slash_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            ( Jansson.array_append_new (x, Jansson.string "a/b")
            ; Assert.assertEqualString "[\"a/b\"]"
                                       (Jansson.dumps x [])
            ; Assert.assertEqualString
                "[\"a\\/b\"]"
                (Jansson.dumps x [Jansson.JSON_ESCAPE_SLASH])
        ))

  fun dumps_ensure_ascii_test () =
      ephemeral
        (Jansson.array ())
        (fn x =>
            ( Jansson.array_append_new (x, Jansson.string "♥")
            ; Assert.assertEqualString "[\"♥\"]"
                                       (Jansson.dumps x [])
            ; Assert.assertEqualString
                "[\"\\u2665\"]"
                (Jansson.dumps x [Jansson.JSON_ENSURE_ASCII])
        ))


  fun dumps_mix_test () =
      ephemeral
        (Jansson.string "♥/")
        (fn x =>
             Assert.assertEqualString
                "\"\\u2665\\/\""
                (Jansson.dumps x [ Jansson.JSON_ENSURE_ASCII
                                 , Jansson.JSON_ENCODE_ANY
                                 , Jansson.JSON_ESCAPE_SLASH ]))

  fun suite _ = Test.labelTests [
    ("object test", object_test),
    ("array test", array_test),
    ("string test", string_test),
    ("integer test", integer_test),
    ("real test", real_test),
    ("true test", true_test),
    ("false test", false_test),
    ("boolean test", boolean_test),
    ("null test", null_test),
    ("string_value test", string_value_test),
    ("string_set test", string_set_test),
    ("integer_value test", integer_value_test),
    ("integer_set test", integer_set_test),
    ("real_value test", real_value_test),
    ("real_set test", real_set_test),
    ("number_value test", number_value_test),
    ("equal test", equal_test),
    ("object_size test", object_size_test),
    ("object_set test", object_set_test),
    ("object_set_new test", object_set_new_test),
    ("object_get test", object_get_test),
    ("object_del test", object_del_test),
    ("object_clear test", object_clear_test),
    ("object_update test", object_update_test),
    ("object_update_existing test", object_update_existing_test),
    ("object_update_missing test", object_update_missing_test),
    ("object_foreach test", object_foreach_test),
    ("array_size test", array_size_test),
    ("array_append test", array_append_test),
    ("array_append_new test", array_append_new_test),
    ("array_get test", array_get_test),
    ("array_set test", array_set_test),
    ("array_set_new test", array_set_new_test),
    ("array_insert test", array_insert_test),
    ("array_insert_new test", array_insert_new_test),
    ("array_remove test", array_remove_test),
    ("array_clear test", array_clear_test),
    ("array_extend test", array_extend_test),
    ("array_foreach test", array_foreach_test),
    ("array_foreachi test", array_foreachi_test),
    ("loads test", loads_test),
    ("loads dup test", loads_dup_test),
    ("loads any test", loads_any_test),
    ("loads eof test", loads_eof_test),
    ("loads int_as_real test", loads_int_as_real_test),
    ("loads mix test", loads_mix_test),
    ("dumps indent test", dumps_indent_test),
    ("dumps any test", dumps_any_test),
    ("dumps escape_slash test", dumps_escape_slash_test),
    ("dumps ensure_ascii test", dumps_ensure_ascii_test),
    ("dumps mix test", dumps_mix_test)
  ]
end

