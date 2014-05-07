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

  fun traverseTest () =
  let
    val files =
      Pathname.traverse (fn x => SOME x) (Pathname.fromPath ".")
      |> List.map Pathname.toString
  in
    (Assert.assertTrue $ List.exists (fn s => s = "./README.mkdn") files;
     Assert.assertTrue $ List.exists (fn s => s = "./.git/config") files)
  end

  fun expandPath_test () =
    let
      val case1 = "/home/hoge/fuga.sml"
      val home =
          case OS.Process.getEnv ("HOME") of SOME home => home | NONE => "~"
    in
      (Assert.assertEqualString case1 (Pathname.expandPath case1);
       Assert.assertEqualString
         (home^"/fuga.sml") (Pathname.expandPath "~/fuga.sml");
       Assert.assertEqualString
         (home^"/piyo/~/buyo.sml") (Pathname.expandPath "~/piyo/~/buyo.sml");
       Assert.assertEqualString "" (Pathname.expandPath ""))
    end

  fun suite _ = Test.labelTests [
    ("fromString", fromStringTest),
    ("fromPath"  , fromPathTest),
    ("traverse"  , traverseTest),
    ("expandPath test", expandPath_test)
  ]
end
