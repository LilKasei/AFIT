(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)

let pow x n =
  if x = 0 && n=0 then
    1
  else
    let rec power x n r =
      if n!=0 then
        power x (n-1) (r*x)
      else
        r
    in
    power x n 1;;

let power x n =
  if n mod 2 = 0 then
    pow (pow x 2) (n/2)
  else
    x*(pow(pow x 2) ((n-1)/2));;



let en_bits integer bits =
  let rec conversion bits integer =
    if bits = -1 then
      []
    else
      let pow = power 2 bits in
      if pow > integer then
        0 ::  conversion (bits-1) integer
      else
        1 :: conversion (bits-1) (integer-pow)
  in conversion (bits-1) integer;;


let str_to_bits str bits =
  let rec calcul acu=
    if acu<= String.length(str) -1 then
      en_bits (int_of_char(str.[acu])) bits @ calcul (acu+1)
    else
      []
  in calcul 0;;

let list_to_dec list long =
  let rec resultat liste long =
      match liste with
          [] -> 0
        |(e::l) -> e* power 2 long + resultat l (long-1)
  in resultat list long;;
    
    

  
let encode str bits =
  let long = (String.length(str) * bits) -1 in
  list_to_dec (str_to_bits str bits) long;;
  
(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
*)

let rec found num puiss =
  if power 2 puiss > num then
    puiss
  else
    found num (puiss+1);;
      
let bits_to_traitement liste bits =
  let rec operation liste bit acu =
    match (liste,bit) with
      ([],_) -> ""
     |(e::l,0) ->Char.escaped(Char.chr(e* power 2 bit+ acu))^ operation l (bits-1) 0 
     |(e::l,bit) -> operation l (bit-1) (e * power 2 bit+ acu) 
  in operation liste (bits-1) 0 ;;     

  
let decode msg bits =
  let number = found msg 0 in
  let chaine_de_bits = en_bits msg number
  in
  bits_to_traitement chaine_de_bits bits;;
  
