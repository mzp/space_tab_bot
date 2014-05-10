structure JsonTest = struct
  structure Test = SMLUnit.Test
  structure Assert = SMLUnit.Assert

  fun toStringTest () =
      let
        val json = Json.Object
                     [ ("title", Json.String "You use tabs and spaces for indent")
                     , ("body" , Json.String "hoge")
                     ]
      in
        Assert.assertEqualString
          "{\"title\": \"You use tabs and spaces for indent\", \"body\": \"hoge\"}"
          (Json.toString json)
      end

  fun suite _ = Test.labelTests [
        ("toString test", toStringTest)
  ]
end
