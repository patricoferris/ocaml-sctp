let ( let* ) = Result.bind
let ( let+ ) v f = Result.map f v
let pad len = 4 - (len mod 4)
