local
  open Base
  structure GO = GetOpt
in

datatype commandLineArgs =
    Help
  | Version
  | DryRun
  | ConfigFilePath of string

datatype runmode =
    PrintHelp
  | PrintVersion
  | Main of {config : string option, dry : bool}

fun printHelp () =
  let
    val helpMessage =
      "Usage: " ^ CommandLine.name () ^ " [options]\n\
      \Options:\n\
      \ -h, --help           show help\n\
      \ -v, --version        show version\n\
      \ -n, --dry-run        print the issues that would be posted, but don't post them\n\
      \ --config=<filepath>  specify config file path. [default=~/.space_tab_bot]\n"
  in
    print helpMessage
  end

fun printVersion () = puts "The Space Tab Bot, version 0.1"

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

fun setRunmode (command, Main (mode as {config, dry})) =
    (case (command, config) of
         (GO.OPTION Help, NONE) => PrintHelp
       | (GO.OPTION Version, NONE) => PrintVersion
       | (GO.OPTION (ConfigFilePath path), NONE)
         => Main (mode # {config = SOME path})
       | (GO.OPTION DryRun, _) => Main (mode # {dry = true})
       | (GO.ARG name, NONE) => failwith ("invalid input `" ^ name ^ "'")
       | _ => failwith "invalid multiple options")
  | setRunmode _ = failwith "invalid multiple options"

val () =
  let
    val args = CommandLine.arguments ()
    val commands = GO.getopt options args
        handle GO.NoArg name =>
               failwith ("option `" ^ name ^ "' requires an argument")
             | GO.HasArg name =>
               failwith ("option `" ^ name ^ "' requires no argument")
             | GO.Unknown name =>
               failwith ("invalid option `" ^ name ^ "'")
    val runmode = Main {config = NONE, dry = false} (* default *)
    val runmode =
        foldl setRunmode runmode commands
  in
    case runmode of
      PrintHelp => printHelp ()
    | PrintVersion => printVersion ()
    | Main {config, dry} =>
      let
        val configPath =
          Option.getOpt (config, Pathname.expandPath "~/.space_tab_bot")
        val () =
            if OS.FileSys.access (configPath, nil) then ()
            else failwith
                   "could not found config file.\n\
                   \Please specify config file by '--config' opiton"
        val urls =
          Setting.readFromFile $ Pathname.fromPath configPath
        val bannedFiles =
          List.map (Detector.detect o Github.clone) urls
      in
        ListPair.zip (urls,bannedFiles)
        |> List.filter (not o List.null o #2)
        |> List.filter (fn (uri,_) =>
            Github.issues uri
            |> List.map #title
            |> List.exists (fn x => x ="You use tabs and spaces for indent")
            |> not)
        |> List.app (if dry then dryRun else report)
      end
  end
    handle Failure message =>
      (puts ("Error: " ^ message); OS.Process.exit OS.Process.failure)
end
