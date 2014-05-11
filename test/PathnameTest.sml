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

  fun chdir_test () =
    let
      val cwd =
        OS.FileSys.getDir ()
      val () =
        Pathname.chdir (fn p => Assert.assertEqualString "/tmp" $ Pathname.toString p) "/tmp"
      val () =
        Pathname.chdir (fn _ => Assert.assertEqualString "/tmp" $ OS.FileSys.getDir()) "/tmp"
      val () =
        Assert.assertEqualString cwd (OS.FileSys.getDir ())
    in
      ()
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

  fun expandPath_home_test () =
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

  fun expandPath_cwd_test () =
    Pathname.chdir
      (fn _ => Assert.assertEqualString "/tmp/hoge.sml" $ Pathname.expandPath "./hoge.sml")
      "/tmp"

  fun map_test () =
    Pathname.fromPath "/tmp/bar"
    |> Pathname.map (fn s => s ^ "/baz")
    |> Pathname.toString
    |> Assert.assertEqualString "/tmp/bar/baz"

  fun suite _ = Test.labelTests [
    ("fromString", fromStringTest),
    ("fromPath"  , fromPathTest),
    ("traverse"  , traverseTest),
    ("chdir"     , chdir_test),
    ("expandPath(home) test", expandPath_home_test),
    ("expandPath(cwd) test", expandPath_cwd_test),
    ("mapTest test", map_test)
  ]
end
