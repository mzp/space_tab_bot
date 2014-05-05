structure PathnameTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert
  open Base

  fun fromStringTest () =
    Pathname.fromString "foo\nbar"
    |> Pathname.openIn TextIO.inputAll
    |> Assert.assertEqualString "foo\nbar"

  fun fromPathTest () =
  let
    val os =
      TextIO.openOut "./test_file.tmp"
    val () =
      TextIO.output (os, "foo\nbar")
    val () =
      TextIO.closeOut os
  in
    Pathname.fromPath "./test_file.tmp"
    |> Pathname.openIn TextIO.inputAll
    |> Assert.assertEqualString "foo\nbar"
  end

  fun suite _ = Test.labelTests [
    ("fromString", fromStringTest),
    ("fromPath"  , fromPathTest)
  ]
end
