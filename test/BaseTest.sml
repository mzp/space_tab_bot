structure BaseTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert
  open Base

  fun id_test () =
    (Assert.assertEqualInt 42 (id 42);
     Assert.assertEqualString  "Thanks for All the Fish" (Base.id "Thanks for All the Fish"))

  fun curry_uncurry_test () =
    let
      fun plus x y = x + y
    in
      (Assert.assertEqualInt (curry (op +) 1 2) (1 + 2);
       Assert.assertEqualInt (uncurry plus (1,2)) (plus 1 2);
       Assert.assertEqualInt ((uncurry (curry (op +))) (1,2)) (1 + 2))
    end

  fun strip_test () =
    (Assert.assertEqualString "foo" (strip "foo");
     Assert.assertEqualString "foo" (strip "foo ");
     Assert.assertEqualString "foo" (strip "foo\r\n");
     Assert.assertEqualString "foo" (strip " foo");
     Assert.assertEqualString "foo" (strip " foo ");
     Assert.assertEqualString "lazy dog" (strip "lazy dog"))

  fun expandPath_test () =
    let
      val full = "/home/hoge/fuga.sml"
      val home = "~/fuga.sml"
    in
      (Assert.assertEqualString full (expandPath full);
       Assert.assertFalse (home = expandPath home))
    end

  fun suite _ = Test.labelTests [
    ("id test", id_test),
    ("curry_uncurry_test", curry_uncurry_test),
    ("strip test", strip_test),
    ("expandPath test", expandPath_test)
  ]
end

