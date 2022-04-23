(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class in the
   built-in integer case.
*)

let rec shift_mult bA d =
   match (bA,d) with
      |(bA,0) -> bA
      |(bA,d) -> 0:: shift_mult bA (d-1);;

let mult_n bA bB =
  let rec mult l q resultat acu=
    match (l,q) with
    |([],q) -> resultat
    |(n::m,q) when n = 1 -> mult m q (add_n (shift_mult q acu) resultat) (acu+1)
    |(_::m,q) -> mult m q resultat (acu+1)
  in mult bA bB [0] 0;;

let pow x n =
  if compare_b x [] = 0 && compare_b n [] =0 then
    [0;1]
  else
    let rec power x n r =
      if compare_b n [] = 0 then
        r
      else
        power x (diff_b n [0;1]) (mult_b r x )
    in power x n [0;1];;
        
      

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
*)
let rec search bitarray x =
  match bitarray with
    |[] -> 0
    |e::l when e = x -> 1
    |_::l -> search l x;;


let power x n = 
  if (mod_b n [0;0;1]) = [] then
    pow (pow x [0;0;1]) (quot_b n [0;0;1])
  else
    mult_b x (pow (pow x [0;0;1]) (quot_b (diff_b n [0;1]) [0;0;1]));;

(* Modular expnonentiation ; modulo a given natural (bitarray without
   sign bits).
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
*)



let mod_po x n m =
    let rec mod_pow x n m =
      if compare_b n [] = 0 then
        [0;1]
      else
        if mod_b n [0;0;1] = [] then
          let z = mod_pow x (quot_b n [0;0;1]) m
          in mod_b (mult_b z z) m
        else mod_b (mult_b (mod_b x m) (mod_pow x (diff_b n [0;1]) m)) m
    in
      from_int(to_int(mod_pow x n m));;
  
let mod_power x n m =
  match(x,n) with
    |(x,_) when x = [0;1] -> mod_b [0;1] m
    |(x,n) when x = [1;1] && mod_b n [0;0;1] = []-> mod_b [0;1] m
    |(x,n) when x = [1;1] && mod_b n [0;0;1] = [0;1] -> mod_b [1;1] m
    |_ ->mod_po x n m;;


(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  if x = [] then
    []
  else
    if n = p then
      mod_power x [0;1] p
    else
      if n = (diff_b p [0;1]) then
        [0;1]
      else
        mod_power x (mod_b n (diff_b p [0;1])) p;;


