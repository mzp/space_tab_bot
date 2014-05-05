structure Setting = struct
  open Base

  fun loop xs instream =
   let
     val line = TextIO.inputLine instream
                |> Option.map strip
   in
     case line of
       NONE =>
          List.rev xs
     | SOME(str) =>
        if str = "" then
          loop xs instream
        else
          loop (Uri.fromString str :: xs) instream
   end

  fun readFromFile path =
    Pathname.openIn (loop []) path
end
