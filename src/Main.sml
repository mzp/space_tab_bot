local
  open Base
  structure GO = GetOpt
in

exception Error of string

datatype commandLineArgs =
    Help
  | Version
  | ConfigFilePath of string

datatype runmode =
    PrintHelp
  | PrintVersion
  | Main of string option

fun printHelp () =
  let
    val helpMessage =
      "Usage: " ^ CommandLine.name () ^ " [options]\n\
      \Options:\n\
      \  --help              show help\n\
      \  --version           show version\n\
      \  --config=<filepath> specify config file path\n"
  in
    print helpMessage
  end

fun printVersion () = Base.puts "The Space Tab Bot, version 0.1"

fun report (url, files) =
  if List.null files then
    ()
  else
    Github.postIssue url "You have space-tab-mixed file"

val options =
  [
    GO.DLONG ("help", GO.NOARG Help),
    GO.DLONG ("version", GO.NOARG Version),
    GO.DLONG ("config", GO.REQUIRED ConfigFilePath)
  ]

fun setRunmode (GO.OPTION Help, Main NONE) = PrintHelp
  | setRunmode (GO.OPTION Version, Main NONE) = PrintVersion
  | setRunmode (GO.OPTION (ConfigFilePath path), Main NONE) = Main (SOME path)
  | setRunmode (GO.ARG name, Main NONE) =
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
    val runmode = foldl setRunmode (Main NONE) commands
  in
    case runmode of
      PrintHelp => printHelp ()
    | PrintVersion => printVersion ()
    | Main pathOpt =>
      let
        val urls =
            Setting.readFromFile $ Pathname.fromString "~/.space_tab_bot"
        val bannedFiles = List.map (Detector.detect o Github.clone) urls
      in
        List.app report $ ListPair.zip (urls,bannedFiles)
      end
  end
    handle Error message =>
      (Base.puts message; OS.Process.exit OS.Process.failure)
end
