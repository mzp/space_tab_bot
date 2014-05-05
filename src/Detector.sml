structure Detector = struct
  open Base

  fun isTab str =
    String.isSubstring "\t" str

  fun isSpace str =
    String.isSubstring " " str

  fun indent str =
    Substring.full str
    |> Substring.takel Char.isSpace
    |> Substring.string


  fun isBoth opt =
    #isTab opt andalso #isSpace opt

  fun isDetect path =
  let
    fun loop opt instream =
    let
      val opt' =
        TextIO.inputLine instream
        |> Option.map indent
        |> Option.map (fn indent =>
            { isTab   = #isTab opt orelse isTab indent,
              isSpace = #isSpace opt orelse isSpace indent})
    in
      case opt' of
        NONE => isBoth opt
      | SOME x => isBoth x orelse loop x instream
    end
  in
    Pathname.openIn (loop { isTab=false, isSpace=false }) path
  end

  fun detect path =
    Pathname.traverse (Option.filter isDetect) path
end
