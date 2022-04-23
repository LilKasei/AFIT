(** Factoring Built-In Int Primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key = let(x,_) = key in
  let rec destruc x arg =
      if (x mod arg) = 0 then
        (arg,x/arg)
      else
        destruc x (arg+1)
  in destruc x 2;
