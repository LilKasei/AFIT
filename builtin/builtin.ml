let sign n =
    if n<0 then -1
    else 1;;

let rec quot a b =
  if a<0 then
    if a mod b = 0 then a/b
    else a/b - 1
  else
    a/b;;

let rec modulo a b=
  if a>=0 then
      if a<b then a
      else modulo (a-b) b
  else
    modulo (a+b) b;;

let div a b= (quot a b, modulo a b);;

