structure Pathname =
struct
  open Base
  datatype t = File of string | StringIO of string

  val fromPath =
    File

  val fromString =
    StringIO

  fun openIn f t =
  let
    val instream =
      case t of
        File path    =>
          TextIO.openIn path
      | StringIO str =>
          TextIO.openString str
    val ret =
      f instream handle e => (TextIO.closeIn instream; raise e)
    val () =
      TextIO.closeIn instream
  in
    ret
  end
end
