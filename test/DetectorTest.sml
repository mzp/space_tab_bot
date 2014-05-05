structure DetectorTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert
  open Base

  fun isDetectTest () =
    (Assert.assertTrue  (Detector.isDetect (Pathname.fromString "  \t"));
     Assert.assertFalse (Detector.isDetect (Pathname.fromString ""));
     Assert.assertFalse (Detector.isDetect (Pathname.fromString "   "));
     Assert.assertFalse (Detector.isDetect (Pathname.fromString "\t\t"));
     Assert.assertTrue (Detector.isDetect (Pathname.fromString "\t  foo"));
     Assert.assertFalse (Detector.isDetect (Pathname.fromString "  foo\tbar"));
     Assert.assertFalse (Detector.isDetect (Pathname.fromString "  foo bar"));
     Assert.assertTrue  (Detector.isDetect (Pathname.fromString "  foo\n\tbar"))
    )

  fun detectTest () =
    Detector.detect (Pathname.fromPath "./test/test_files")
    |> List.map Pathname.toString
    |> Assert.assertEqualStringList ["./test/test_files/Bad.sml"]

  fun suite _ = Test.labelTests [
    ("isDetect test", isDetectTest),
    ("detect test", detectTest)
  ]
end
