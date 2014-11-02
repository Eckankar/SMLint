fun foo xs = case xs of
      []      => 0
    | [y::ys] => 1 + foo ys

fun bar [x::xs] = x @ xs
