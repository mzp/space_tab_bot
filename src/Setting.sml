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
          case Uri.fromString str of
              SOME uri => loop (uri :: xs) instream
            | NONE => loop xs instream (* ignore invalid uri *)
   end

  fun readFromFile path =
    Pathname.openIn (loop []) path
end
