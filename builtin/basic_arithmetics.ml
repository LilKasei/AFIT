(** Basic arithmetics with built-in integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
**)
let gcd a b =
  let x = sign a in
  let y = sign b in
  let rec pgcd a b n=
    if a mod n =0 && b mod n = 0 then
      n * sign x * sign y
    else
      pgcd a b (n-1)
  in
  if a*x > b*x then
    pgcd a b (b*x)
  else
    pgcd b a (a*x);;
 

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
**)


let bezout a b =
  let rec bez r u v rp up vp=
    if rp=0 then (u,v,r)
    else
      bez rp up vp (r-((r/rp)*rp)) (u-((r/rp)*up)) (v-((r/rp)*vp))
in bez a 1 0 b 0 1;;


