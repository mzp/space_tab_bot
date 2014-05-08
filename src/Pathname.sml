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

  fun ltrim str =
    String.substring (str, 1, size str - 1)

  fun expandPath input =
    (case String.sub (input, 0) of
         #"~" =>
         (case OS.Process.getEnv "HOME" of
              NONE => input
            | SOME home => home ^ ltrim input)
       | #"." =>
           OS.FileSys.getDir () ^ ltrim input
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

  fun map f (File path)  = File (f path)
    | map f (x as StringIO _) = x

  fun chdir f dir =
    let
      val cwd = OS.FileSys.getDir ()
      fun finnally _ =
        OS.FileSys.chDir cwd
      val () =
        OS.FileSys.chDir dir
    in
      protectx (fromPath dir) f finnally
    end

  val prim_fnmatch = _import "fnmatch" : (string, string, int) -> int
  val prim_puts = _import "puts" : string -> int

  fun fnmatch pat path =
    0 = prim_fnmatch (pat, toString path, 0)
end
