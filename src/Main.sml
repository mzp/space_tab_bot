let
  val urls = Setting.readFromFile (Pathname.fromString "~/.space_tab_bot")
  val repos = List.map Github.clone urls
  val bannedFiles = List.map Detector.detect repos
  fun report (url, files) = Github.postIssue url "You have space-tab-mixed file"
in
  List.app report (Base.zip urls bannedFiles)
end
