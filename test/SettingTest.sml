structure SettingTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert
  open Base

  fun assertEqual x y =
    Assert.assertTrue (x = y)

  fun readFromFileTest () =
  let
    val urls =
      Pathname.fromString
      "http://github.com/mzp/space_tab_bot\nhttps://github.com/codefirst/AsakusaSatellite\n\r\n"
      |> Setting.readFromFile
      |> List.map Uri.toString
    val expect =
      [ "http://github.com/mzp/space_tab_bot",
        "https://github.com/codefirst/AsakusaSatellite"]
  in
    Assert.assertEqualStringList expect urls
  end

  fun suite _ = Test.labelTests [
    ("readFromFile", readFromFileTest)
  ]
end
