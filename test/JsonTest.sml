structure JsonTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert
  open Json

  fun toStringTest () =
      let
        val json = Json.Object
                     [ ("title", Json.String "You use tabs and spaces for indent")
                     , ("body" , Json.String "hoge")
                     ]
      in
        Assert.assertEqualString
          "{\"title\": \"You use tabs and spaces for indent\", \"body\": \"hoge\"}"
          (Json.toString json)
      end

  fun assert x y =
    Assert.assertTrue (x = y)

  fun boolTest () =
    (assert (SOME true)  (bool True);
     assert (SOME false) (bool False);
     assert NONE (bool Null))

  fun nullTest () =
    (assert (SOME ()) (null Null);
     assert NONE (null (Integer 42)))

  fun stringTest () =
    (assert (SOME "hello") (string (String "hello"));
     assert NONE (string (Integer 42)))

  fun integerTest () =
    (assert (SOME 42) (integer (Integer 42));
    assert  NONE (integer Null))

  fun realTest () =
    let
      open Real
      infix 1 ==
    in
      Assert.assertTrue  (42.0 == (Option.valOf (real_ (Real 42.0))));
      Assert.assertFalse (Option.isSome (real_ Null))
    end

  fun objectTest () =
    let
      fun assertJson x y =
        Assert.assertTrue (Json.toString x = Json.toString y)
      fun assertObject xs ys =
        Assert.assertEqualList (fn x => fn y =>
          (Assert.assertEqualString (#1 x) (#1 y);
          assertJson (#2 x) (#2 y)))
          xs ys
      val xs =
        [("foo", Integer 0), ("bar", String "hoge")]
    in
      assertObject [] (Option.valOf (object (Object [])));
      assertObject xs (Option.valOf (object (Object xs)));
      Assert.assertFalse (Option.isSome (object Null))
    end

  fun arrayTest () =
    let
      fun assertJson x y =
        Assert.assertTrue (Json.toString x = Json.toString y)
      fun assertArray xs ys =
        Assert.assertEqualList assertJson xs ys
      val xs =
        [Integer 0, Integer 42]
    in
      assertArray xs (Option.valOf (array (Array xs)));
      assertArray [] (Option.valOf (array (Array [])));
      Assert.assertFalse (Option.isSome (array Null))
    end

  fun suite _ = Test.labelTests [
    ("toString test", toStringTest),
    ("bool test", boolTest),
    ("null test", nullTest),
    ("string test", stringTest),
    ("integer test", integerTest),
    ("real test", realTest),
    ("object test", objectTest),
    ("array test", arrayTest)
  ]
end
