structure BaseTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert
  open Base

  fun assertEqual x y =
    Assert.assertTrue (x = y)

  fun id_test () =
    (assertEqual 42 (id 42);
     assertEqual  "Thanks for All the Fish" (Base.id "Thanks for All the Fish"))

  fun strip_test () =
    (Assert.assertEqualString "foo" (strip "foo");
     Assert.assertEqualString "foo" (strip "foo ");
     Assert.assertEqualString "foo" (strip "foo\r\n");
     Assert.assertEqualString "foo" (strip " foo");
     Assert.assertEqualString "foo" (strip " foo ");
     Assert.assertEqualString "lazy dog" (strip "lazy dog"))

  fun suite _ = Test.labelTests [
    ("id test", id_test),
    ("strip test", strip_test)
  ]
end

