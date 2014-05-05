structure Base = struct
  fun id x = x
  fun op $ (f, x) = f x
  fun op |> (x, f) = f x
end
