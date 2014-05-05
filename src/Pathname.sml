structure Pathname =
struct
  type t = string
  val fromString = Base.id
end
