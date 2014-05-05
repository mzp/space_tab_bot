open Base

val suites = SMLUnit.Test.TestList [
  BaseTest.suite (),
  PathnameTest.suite ()
]

val () =
  SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suites

val () = OS.Process.exit OS.Process.success
