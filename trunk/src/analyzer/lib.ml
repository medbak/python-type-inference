let rec repeat elt n = match n with
  | 0 -> []
  | n -> elt::(repeat elt (n-1))

let rec range n m =
  if n > m then []
  else n :: (range (n + 1) m)
    
    
