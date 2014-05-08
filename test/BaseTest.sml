structure BaseTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert
  open Base

  fun id_test () =
    (Assert.assertEqualInt 42 (id 42);
     Assert.assertEqualString  "Thanks for All the Fish" (Base.id "Thanks for All the Fish"))

  fun apply_test () =
    let
      fun f x = x +1
      fun g x = 2 * x
      fun h x = 3 * x
    in
      Assert.assertEqualInt (f (g (h 10))) (f $ g $ h 10)
    end

  fun curry_uncurry_test () =
    let
      fun plus x y = x + y
    in
      (Assert.assertEqualInt (curry (op +) 1 2) (1 + 2);
       Assert.assertEqualInt (uncurry plus (1,2)) (plus 1 2);
       Assert.assertEqualInt ((uncurry (curry (op +))) (1,2)) (1 + 2))
    end

  fun tee_test () =
    Assert.assertEqualInt 42 $ tee (fn n => Assert.assertEqualInt 42 n) 42

  fun const_test () =
    Assert.assertEqualInt 42 (const 42 "Thanks for All the Fish")

  fun flip_test () =
    let
      fun f x y = x - y
    in
      Assert.assertEqualInt 5 (flip f 5 10)
    end

  fun strip_test () =
    (Assert.assertEqualString "foo" (strip "foo");
     Assert.assertEqualString "foo" (strip "foo ");
     Assert.assertEqualString "foo" (strip "foo\r\n");
     Assert.assertEqualString "foo" (strip " foo");
     Assert.assertEqualString "foo" (strip " foo ");
     Assert.assertEqualString "lazy dog" (strip "lazy dog"))

  fun takeWhile_test () =
    (Assert.assertEqualIntList [] $ takeWhile (fn x => x < 4) [];
     Assert.assertEqualIntList [] $ takeWhile (fn x => x < 4) [100];
     Assert.assertEqualIntList [1,2,3] $ takeWhile (fn x => x < 4) [1,2,3,4,5])

  fun dropWhile_test () =
    (Assert.assertEqualIntList [] $ dropWhile (fn x => x < 4) [];
     Assert.assertEqualIntList [100] $ dropWhile (fn x => x < 4) [100];
     Assert.assertEqualIntList [4,5] $ dropWhile (fn x => x < 4) [1,2,3,4,5])

  fun span_test () =
    (Assert.assertTrue $ ([],[]) = span (fn x => x < 4) [];
     Assert.assertTrue $ ([1,2,3],[4,5]) = span (fn x => x < 4) [1,2,3,4,5])

  fun suite _ = Test.labelTests [
    ("id test", id_test),
    ("apply test", apply_test),
    ("curry_uncurry_test", curry_uncurry_test),
    ("tee test", tee_test),
    ("const test", const_test),
    ("flip test", flip_test),
    ("strip test", strip_test),
    ("takeWhile test", takeWhile_test),
    ("dropWhile test", dropWhile_test),
    ("span test", span_test)
  ]
end
