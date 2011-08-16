type t = int
let value = ref 0
let get () = incr value; !value
  
