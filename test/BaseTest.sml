structure BaseTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert

  fun assertEqual x y =
    Assert.assertTrue (x = y)

  fun zip_empty () =
    assertEqual [] (Base.zip [] [])

  fun suite _ = Test.labelTests [
    ("zip empty case", zip_empty)
  ]
end

