structure Detector = struct
  open Base

  val ignoreDirectories =
    ["*/.git"]
  val ignoreFiles =
    ["*.png", "*.exe"]

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

  fun isDetected path =
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

  fun isNotMatch pats path =
    List.all (not o flip Pathname.fnmatch path) pats

  fun detect path =
    let
      val path' = Pathname.map Pathname.expandPath path
      fun f x =
        isNotMatch ignoreFiles x andalso isDetected x
    in
      Pathname.traverse (isNotMatch ignoreDirectories) (Option.filter f) path'
      |> List.map (Pathname.map (fn s=>
          ( OS.Path.mkRelative { path=s, relativeTo=Pathname.toString path'})))
    end
end
