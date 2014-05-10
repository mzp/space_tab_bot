infix 0 |>

structure Base = struct
  exception Undefine
  exception Failure of string

  fun id x = x
  fun op $ (f, x) = f x
  fun op |> (x, f) = f x
  fun undef () = raise Undefine
  fun puts str = (print str; print "\n")
  fun curry f x y = f (x,y)
  fun uncurry f (x,y) = f x y

  fun failwith str = raise (Failure str)

  fun strip str =
    Substring.full str
    |> Substring.dropl Char.isSpace
    |> Substring.dropr Char.isSpace
    |> Substring.string

  fun protectx v f cleanup =
      let
        val res = f v
      in
        (cleanup v; res)
      end handle e => (cleanup v; raise e)

  fun span _ [] =
        ([], [])
    | span p (xss as (x::xs)) =
        if p x then
          let
            val (ys,zs) =
              span p xs
          in
            (x::ys, zs)
          end
        else
          ([], xss)

  fun takeWhile p xs =
    #1 (span p xs)
  fun dropWhile p xs =
    #2 (span p xs)
end
