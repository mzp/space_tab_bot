open Base

fun report (url, files) =
  if List.null files then
    ()
  else
    Github.postIssue url "You have space-tab-mixed file"

val urls =
  Setting.readFromFile $ Pathname.fromString "~/.space_tab_bot"

val bannedFiles =
  List.map (Detector.detect o Github.clone) urls

val () =
  List.app report $ ListPair.zip (urls,bannedFiles)
