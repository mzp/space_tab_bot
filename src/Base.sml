structure Base = struct
  exception Undefine

  fun id x = x
  fun op $ (f, x) = f x
  fun op |> (x, f) = f x
  fun undef () = raise Undefine
end
