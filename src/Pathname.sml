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

  fun withResource open_ close f x =
  let
    val resource =
      open_ x
    val ret =
      f resource handle e => (close resource; raise e)
    val () =
      close resource
  in
    ret
  end

  fun opt NONE = []
    | opt (SOME x) = [x]

  fun openIn f t =
  let
    fun open_ (File path) = TextIO.openIn path
      | open_ (StringIO str) = TextIO.openString str
  in
    withResource open_ TextIO.closeIn f t
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
    withResource OS.FileSys.openDir OS.FileSys.closeDir (loop []) str
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

end
