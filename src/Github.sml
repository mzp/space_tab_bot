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

  fun postIssue url text = ()
end
