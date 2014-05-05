structure Uri = struct
  open Base

  type t = string

  val fromString =
    Base.id

  val toString =
    Base.id
end
