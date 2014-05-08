structure Message = struct
  open Base

  fun issues files =
    let
      val body =
        files
        |> List.map (fn path => " * " ^ Pathname.toString path)
        |> String.concatWith "\n"
      val header =
        "![TabSpacesBoth](https://raw.github.com/mzp/space_tab_bot/master/misc/TabsSpacesBoth.png)\n\n"
    in
      header ^ body
    end
end
