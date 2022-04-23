(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test **)
let is_prime n = 
  let rec second p= match p with
      1 -> true
    |p when (n mod p !=0) && (p!=1) -> second (p-1)
    |_->false
  in second (n-1);;

(** Primality test based on smalle Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 **)
let  is_pseudo_prime p test_seq =
  let rec pseudo l= match l with
      []->true
    |e::l -> (mod_power e p p = modulo e p) && pseudo l
  in if p<=1 then false
    else pseudo test_seq;;

