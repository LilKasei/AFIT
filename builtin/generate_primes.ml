(** Generating primes *)

open Builtin
open Basic_arithmetics

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)
let init_eratosthenes n =
  let rec generate e = 
    match e with 
    | e when e = n+1 -> []
    | e when e=2  -> 2:: generate (e+1)
    | e when (e mod 2 != 0) -> e :: generate (e+1)
    |_ -> generate (e+1)
  in generate 2;;

let suppression_multiples n liste =  
  let rec suppr count liste  =
    match (liste,count) with 
    | ([],_) -> []
    | (e::l,_) when (e mod n = 0) && (e!= n) -> suppr (count+1) l
    | (e::l,_) -> e :: suppr (count+1) l
  in suppr 3 liste;;
  



(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
**)
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

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file in
  let rec write = function
     [] -> close_out oc
    |e::l -> Printf.fprintf oc "%i/n" e
  in write li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = () ;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None ;;

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c = () ;;

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = [] ;;

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty "
  | e::[] -> e
  | h::t -> last_element t ;;

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t;;

(* Generating couples of prime numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
*)

let double_primes limit isprime =
  let primes = eratosthenes limit in
  let rec test primes =
    match primes with 
    | [] -> []
    | e :: l when isprime ((e*2)+1) = true -> (e,(e*2+1)):: test l
    |_::l -> test l
  in test primes;;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let primes = eratosthenes limit in
  let rec test primes =
    match primes with 
      | [] -> []
      | e :: l when e =2 -> test l
      | e :: l when isprime (e+2) = true -> (e,(e+2)):: test l
      |_::l -> test l
  in test primes;;


