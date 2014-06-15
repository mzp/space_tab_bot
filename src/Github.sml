structure Github = struct
  open Base

  exception CommandError of OS.Process.status

  fun clone uri =
    let
      fun isDir s =
        OS.FileSys.isDir s handle OS.SysErr _ => false
      val reposPath =
        case #path uri of
          SOME(path) =>
            Pathname.fromPath ("./github_repos/" ^ path)
        | NONE =>
            failwith "invalid url"
      val () =
        Pathname.mkDirP reposPath
      val command =
        if isDir (Pathname.toString reposPath ^ "/.git") then
          "git pull"
        else
          "git clone " ^ Uri.toString uri ^ " ."
      val status =
        Pathname.chdir
          (fn _ => OS.Process.system command)
          (Pathname.toString reposPath)
    in
      if OS.Process.isSuccess status then
        reposPath
      else
        failwith ("command \"" ^ command ^ "\" is failed")
    end

  fun word_to_string xs =
    Byte.bytesToString xs
    |> puts

  fun times 0 _ x = x
    | times n f x = f (times (n-1) f x)

    (* FIXME: Too much adhoc. Use URI parse module *)
  fun repos uri =
    String.explode (Uri.toString uri)
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
            failwith "could not found github account information.\n\
                     \Please set 'GITHUB_USERNAME' and 'GITHUB_PASSWORD'"
      val repos =
        repos url
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

  fun issues uri =
    let
      val repos =
        repos uri
      val apiEntryPoint =
        "https://api.github.com/repos/" ^ repos ^ "/issues"
      fun find (key : string) (xs : (string * Json.t) list) : string option =
        Option.mapPartial Json.string $ assoc key xs
      val issues =
        Http.httpGet Http.emptyOptions apiEntryPoint
        |> Byte.bytesToString
        |> Json.parseString
        |> Json.array
        |> Option.map (List.mapPartial Json.object)
        |> Option.map (List.mapPartial (fn xs =>
           case (find "title" xs, find "body" xs) of
                (SOME title, SOME body) =>
                  SOME {title=title, body=body}
              | _ =>
                  NONE))
    in
      Option.getOpt (issues, [])
    end

end
