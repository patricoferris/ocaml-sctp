let ( let* ) = Result.bind
let ( let+ ) v f = Result.map f v

let pad len =
  let m = len mod 4 in
  if m = 0 then 0 else 4 - m
