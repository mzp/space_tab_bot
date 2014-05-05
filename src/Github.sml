structure Github =
struct
  fun clone _ = Pathname.fromString "/tmp/x"
  fun postIssue url text = ()
end
