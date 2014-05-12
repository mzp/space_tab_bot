local
  open Base
  structure GO = GetOpt
in

exception Error of string

datatype commandLineArgs =
    Help
  | Version
  | DryRun
  | ConfigFilePath of string

datatype runmode =
    PrintHelp
  | PrintVersion
  | Main of string option * { dryRun : bool }

fun printHelp () =
  let
    val helpMessage =
      "Usage: " ^ CommandLine.name () ^ " [options]\n\
      \Options:\n\
      \ -h, --help           show help\n\
      \ -v, --version        show version\n\
      \ -n, --dry-run        print the issues that would be posted, but do not post tem\n\
      \ --config=<filepath>  specify config file path\n"
  in
    print helpMessage
  end

fun printVersion () = Base.puts "The Space Tab Bot, version 0.1"

fun report (url, files) =
  Github.postIssue url $ Message.issues files

fun dryRun (url, files) =
  puts $ Message.issues files

val options =
  [
    GO.SHORT (#"h", GO.NOARG Help),
    GO.DLONG ("help", GO.NOARG Help),
    GO.SHORT (#"v", GO.NOARG Version),
    GO.DLONG ("version", GO.NOARG Version),
    GO.SHORT (#"n", GO.NOARG DryRun),
    GO.DLONG ("dry-run", GO.NOARG DryRun),
    GO.DLONG ("config", GO.REQUIRED ConfigFilePath)
  ]

fun setRunmode (GO.OPTION Help, Main (NONE, _)) = PrintHelp
  | setRunmode (GO.OPTION Version, Main (NONE, _)) = PrintVersion
  | setRunmode (GO.OPTION (ConfigFilePath path), Main (NONE, opt)) = Main (SOME path, opt)
  | setRunmode (GO.OPTION DryRun, Main(path,opt)) = Main (path, opt # { dryRun = true })
  | setRunmode (GO.ARG name, Main (NONE, _)) =
    raise Error ("invalid input `" ^ name ^ "'")
  | setRunmode _ = raise Error ("invalid multiple options")

val () =
  let
    val args = CommandLine.arguments ()
    val commands = GO.getopt options args
        handle GO.NoArg name =>
               raise Error ("option `" ^ name ^ "' requires an argument")
             | GO.HasArg name =>
               raise Error ("option `" ^ name ^ "' requires no argument")
             | GO.Unknown name =>
               raise Error ("invalid option `" ^ name ^ "'")
    val runmode = foldl setRunmode (Main (NONE, { dryRun = false })) commands
  in
    case runmode of
      PrintHelp => printHelp ()
    | PrintVersion => printVersion ()
    | Main (pathOpt, opt) =>
      let
        val configPath =
          Option.getOpt (pathOpt, Pathname.expandPath "~/.space_tab_bot")
        val urls =
          Setting.readFromFile $ Pathname.fromPath configPath
        val bannedFiles =
          List.map (Detector.detect o Github.clone) urls
      in
        ListPair.zip (urls,bannedFiles)
        |> List.filter (not o List.null o #2)
        |> List.app (if #dryRun opt then dryRun else report)
      end
  end
    handle Error message =>
      (Base.puts message; OS.Process.exit OS.Process.failure)
end
