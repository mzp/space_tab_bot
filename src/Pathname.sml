structure Pathname =
struct
  open Base
  datatype t = File of string | StringIO of string

  val fromPath =
    File

  val fromString =
    StringIO

  fun toString (File s) = s
    | toString (StringIO _) = "<string-io>"

  fun opt NONE = []
    | opt (SOME x) = [x]

  fun openIn f t =
  let
    fun open_ (File path)    = TextIO.openIn path
      | open_ (StringIO str) = TextIO.openString str
  in
    Base.protectx (open_ t) f TextIO.closeIn
  end

  fun listFiles str =
  let
    fun loop xs dir =
      case OS.FileSys.readDir dir of
        NONE =>
          xs
      | SOME "." =>
          loop xs dir
      | SOME ".." =>
          loop xs dir
      | SOME file =>
          loop (OS.Path.joinDirFile {dir=str, file=file} :: xs) dir
  in
    Base.protectx (OS.FileSys.openDir str) (loop []) OS.FileSys.closeDir
  end

  fun traverse f (StringIO _) = []
    | traverse f (File path) =
    let
      fun f' x =
        f (fromPath x) |> opt
      fun walk path =
        if OS.FileSys.isDir path then
          listFiles path
          |> List.map walk
          |> List.concat
        else
          f' path
    in
      walk path
    end

  fun expandPath input =
    (case String.sub (input, 0) of
         #"~" =>
         (case OS.Process.getEnv "HOME" of
              NONE => input
            | SOME home => home ^ String.substring (input, 1, size input - 1))
       | _ => input)
    handle Subscript => input (* empty string case *)

  fun tmpDir () =
    let
      val tmp =
        OS.FileSys.tmpName ()
      val () =
        OS.FileSys.remove tmp
      val () =
        OS.FileSys.mkDir tmp
    in
      fromPath tmp
    end

end
