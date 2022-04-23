(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
  let(x,_) = key in
  let rec add k =
    let kk = Scalable.add_b x [0;0;1] in
    if Scalable.mod_b x kk = [] then
      kk
    else
      add kk in
  let rec destruc x arg =
      if (Scalable.mod_b x arg) = [] then
        (Scalable.div_b x arg)
      else
        destruc x (add arg)
  in destruc x [0;1;1];;

