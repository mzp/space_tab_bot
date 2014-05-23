structure UriTest =
struct
  local
    structure Test = SMLUnit.Test
    structure Assert = SMLUnit.Assert
    open Base
  in

  val str1 = "http://hoge.fuga/"
  val uri1 =
      {
        scheme = "http",
        userInfo = NONE,
        host = "hoge.fuga",
        port = NONE,
        path = NONE,
        query = NONE,
        frag = NONE
      } : Uri.t
  val str2 = "foo://example.com:8042/over/there?name=ferret#nose"
  val uri2 =
      {
        scheme = "foo",
        userInfo = NONE,
        host = "example.com",
        port = SOME 8042,
        path = SOME "over/there",
        query = SOME "name=ferret",
        frag = SOME "nose"
      }
  val str3 =
      "http://space:tab@github.com/bot.git?space=yes&tab=yes&both=yes#kill"
  val uri3 =
      {
        scheme = "http",
        userInfo = SOME "space:tab",
        host = "github.com",
        port = SOME 8080,
        path = SOME "bot.git",
        query = SOME "space=yes&tab=yes&both=yes",
        frag = SOME "kill"
      }
  (* val uriStr4 = "urn:example:animal:ferret:nose" *)
  (* val uri4 =  *)

  fun toString_test () =
      (Assert.assertEqualString str1 (Uri.toString uri1);
       Assert.assertEqualString str2 (Uri.toString uri2);
       Assert.assertEqualString str3 (Uri.toString uri3))

  fun fromString_test () =
      (Assert.assertTrue $ SOME uri1 = Uri.fromString str1;
       Assert.assertTrue $ SOME uri2 = Uri.fromString str2;
       Assert.assertTrue $ SOME uri3 = Uri.fromString str3)

  fun toFrom_test () =
    let
      fun toString (SOME uri) = Uri.toString uri
        | toString NONE = ""
    in
      (Assert.assertEqualString str1 (toString $ Uri.fromString str1);
       Assert.assertEqualString str2 (toString $ Uri.fromString str2);
       Assert.assertEqualString str3 (toString $ Uri.fromString str3))
    end

  fun suite _ = Test.labelTests
    [
      ("toString test", toString_test),
      ("fromString test", fromString_test),
      ("toFrom test", toFrom_test)
    ]
  end
end

