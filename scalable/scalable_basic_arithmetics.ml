(** Basic arithmetics for ordered euclidian ring. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let rec search bitarray x =
  match bitarray with
    |[] -> 0
    |e::l when e = x -> 1
    |_::l -> search l x;;

let gcd_b bA bB =
  let x = sign_b bA in
  let y = sign_b bB in
  let rec pgcd a b n =
     if search (mod_b a n) 1 = 0 && search (mod_b b n) 1 = 0 then
        match (x,y) with
         |(1,1) -> diff_b (0 :: n) [0;1]
         |(-1,-1) -> diff_b (0 :: n) [0;1]
         |_ -> diff_b (1 :: n) [0;1]
    else
      pgcd a b (diff_n n [1])
  in
  if (>>) (abs_b(bA)) ( abs_b(bB)) then
    match (bA,bB) with
      |(e::l,r::q) -> pgcd l q q
      |_ -> []
  else
     match (bA,bB) with 
     |(e::l,r::q) -> pgcd q l l
     |_ -> [];;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)

let bezout_b bA bB =
  let r = bA in
  let u = [0;1] in
  let v = [] in
  let r_prime = bB in
  let u_prime = [] in
  let v_prime = [0;1] in
  let rec calcul r u v r_prime u_prime v_prime =
    if r_prime = [] then
      ((from_int(to_int u)),(from_int(to_int v)),(from_int(to_int r)))
    else
      calcul r_prime u_prime v_prime (diff_b r (mult_b (quot_b r r_prime) r_prime))
	(diff_b u (mult_b (quot_b r r_prime) u_prime)) (diff_b v (mult_b (quot_b r r_prime) v_prime))
  in  calcul r u v r_prime u_prime v_prime;;
