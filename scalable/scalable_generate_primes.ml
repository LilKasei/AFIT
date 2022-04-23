(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(* Initializing list of bitarrays for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
  let n = from_int n in 
  let rec generate e =  match e with
      | e when e = (add_b n [0;1]) -> []
      | e when e= [0;0;1] -> [0;0;1] :: generate (add_b e [0;1])
      | e when ( mod_b e [0;0;1] != []) -> e :: generate (add_b e [0;1])
      |_ -> generate (add_b e [0;1])
  in generate [0;0;1];;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)



let suppression_multiples n liste =  (*use for erathosthenes*)
  let n = from_int n in
  let rec suppr count liste  =
    match (liste,count) with 
    | ([],_) -> []
    | (e::l,_) when (mod_b e n = []) && (compare_b e n !=0) -> suppr (count+1) l
    | (e::l,_) -> e :: suppr (count+1) l
  in suppr 3 liste;;


let eratosthenes n =
  let liste = init_eratosthenes n in
  let x = int_of_float(sqrt(float_of_int(n))) + 1 in
  let rec traitement e liste=
    if e <= x then 
      traitement (e+1) (suppression_multiples e liste) 
    else 
      liste 
  in traitement 3 liste;;

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = ()

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = ()

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime bitarrays for specific or fun
   purposes.
 *)

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)
let double_primes limit isprime =
  let primes = eratosthenes limit in
  let rec test primes =
    match primes with
      |[] -> []
      |e::l when isprime (add_b (mult_b e [0;0;1]) [0;1]) = true -> (e,add_b(mult_b e [0;0;1]) [0;1]) :: test l
      |_::l -> test l
  in test primes;;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime = 
  let primes = eratosthenes limit in
  let rec test primes =
    match primes with
      |[] -> []
      |e::l when e = [0;0;1] -> test l
      |e::l when isprime (add_b e [0;0;1]) = true -> (e,(add_b e [0;0;1])) :: test l
      |_ ::l -> test l
  in test primes;;
