open Base

val suites = SMLUnit.Test.TestList [
  BaseTest.suite (),
  PathnameTest.suite (),
  SettingTest.suite (),
  DetectorTest.suite (),
  JanssonTest.suite (),
  JsonTest.suite (),
  UriTest.suite ()
]

val () =
  SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suites

val () = OS.Process.exit OS.Process.success
