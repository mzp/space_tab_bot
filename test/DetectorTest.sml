structure DetectorTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert
  open Base

  fun isDetectedTest () =
    (Assert.assertTrue  (Detector.isDetected (Pathname.fromString "  \t"));
     Assert.assertFalse (Detector.isDetected (Pathname.fromString ""));
     Assert.assertFalse (Detector.isDetected (Pathname.fromString "   "));
     Assert.assertFalse (Detector.isDetected (Pathname.fromString "\t\t"));
     Assert.assertTrue (Detector.isDetected (Pathname.fromString "\t  foo"));
     Assert.assertFalse (Detector.isDetected (Pathname.fromString "  foo\tbar"));
     Assert.assertFalse (Detector.isDetected (Pathname.fromString "  foo bar"));
     Assert.assertTrue  (Detector.isDetected (Pathname.fromString "  foo\n\tbar"))
    )

  fun detectTest () =
    Detector.detect (Pathname.fromPath "./test/test_files")
    |> List.map Pathname.toString
    |> Assert.assertEqualStringList ["Bad.sml"]

  fun suite _ = Test.labelTests [
    ("isDetected test", isDetectedTest),
    ("detect test", detectTest)
  ]
end
