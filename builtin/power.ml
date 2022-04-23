

(** Power function implementations for built-in integers *)

open Builtin
open Basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
*)



let rec pow x n = match n with
    n when n<0 -> invalid_arg "n must be positive"
  | 0 -> 1
  | _ -> x * pow x (n-1);;



(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
*)



let rec power x n = match n with
    n when n<0 -> invalid_arg "n must be positive"
  | 0 -> 1
  | 1 -> x
  | n when modulo n 2 = 0 -> power (x * x) (quot n 2)
  | _ -> x * power (x * x) (quot n 2);;
  


(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
*)


let rec mod_power x n m = match n with
    n when n < 0 -> invalid_arg "n must be positive"
  | 0 -> 1
  | n when modulo n 2 = 0 -> modulo (mod_power (modulo (x*x) m) (quot n 2) m) m
  | _ -> modulo (x * (mod_power (modulo (x*x) m) (quot n 2) m)) m;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
*)


  
let prime_mod_power x n p = match x with
          0  -> 0
         | _ -> mod_power x (modulo n (p - 1)) p;;
