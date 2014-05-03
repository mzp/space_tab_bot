local
  open Base
  structure GO = GetOpt
in

exception Error of string

datatype commandLineArgs =
    Help
  | Version
  | ConfigFilePath of string

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

fun printVersion () = print "The Space Tab Bot, version 0.1\n"

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

val () =
  let
    val args = CommandLine.arguments ()
    val commands = GO.getopt options args
        handle GO.NoArg name =>
          raise Error ("option `" ^ name ^ "' requires an argument")
        handle GO.HasArg name =>
          raise Error ("option `" ^ name ^ "' requires no argument")
        handle GO.Unknown name =>
          raise Error ("invalid option `" ^ name ^ "'")
    val urls =
      Setting.readFromFile $ Pathname.fromString "~/.space_tab_bot"
    val bannedFiles =
      List.map (Detector.detect o Github.clone) urls
  in
    case commands of
        [GO.OPTION Help] => printHelp ()
      | [GO.OPTION Version] => printVersion ()
      | [GO.OPTION (ConfigFilePath path)] => 
        List.app report $ ListPair.zip (urls,bannedFiles)
      | nil => 
        List.app report $ ListPair.zip (urls,bannedFiles)
      | [GO.ARG name] => raise Error ("invalid option `" ^ name ^ "'")
      | _ => raise Error ("invalid multiple options")
  end
    handle Error message => print (message ^ "\n")
end
