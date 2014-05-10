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
    ("loads test", loads_test),
    ("loads dup test", loads_dup_test),
    ("loads any test", loads_any_test),
    ("loads eof test", loads_eof_test),
    ("loads int_as_real test", loads_int_as_real_test),
    ("loads mix test", loads_mix_test)
  ]
end

