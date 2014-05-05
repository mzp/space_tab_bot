val () = SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut}
  (BaseTest.suite ())

val () = OS.Process.exit OS.Process.success
