structure Github = struct
  open Base

  exception CommandError of OS.Process.status

  fun clone uri =
    let
      val tmpDir =
        Pathname.tmpDir ()
      val command =
        "git clone " ^ Uri.toString uri ^ " " ^ Pathname.toString tmpDir
      val status =
        OS.Process.system command
    in
      if OS.Process.isSuccess status then
        tmpDir
      else
        raise (CommandError status)
    end

  fun word_to_string xs =
    Byte.bytesToString xs
    |> puts

  fun times 0 _ x = x
    | times n f x = f (times (n-1) f x)

    (* FIXME: Too much adhoc. Use URI parse module *)
  fun repos uri =
    String.explode uri
    |> times 3 (tl o dropWhile (fn c => c <> #"/"))
    |> String.implode

  fun postIssue url text =
    let
      val (username, password) =
        case (OS.Process.getEnv "GITHUB_USERNAME", OS.Process.getEnv
          "GITHUB_PASSWORD") of
         (SOME username, SOME password) =>
            (username, password)
        | _ =>
            failwith "could not found github account information.\nPlease set 'GITHUB_USERNAME' and 'GITHUB_PASSWORD'"
      val repos =
        repos (Uri.toString url)
      val apiEntryPoint =
        "https://api.github.com/repos/" ^ repos ^ "/issues"
      (* FIXME: use some JSON library *)
      val escape =
        String.translate (fn c =>
        case c of
             #"\n" => "\\n"
           | #"\"" => "\\\""
           | _ => Char.toString c)
      val json =
        "{\"title\": \"You use tabs and spaces for indent\", \"body\":\""^escape text^"\"}"
    in
      Http.httpPost
        {username=SOME username, password=SOME password }
        apiEntryPoint json
      |> word_to_string
    end
end
