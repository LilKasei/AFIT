(** Ciphers
    Built-in integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
 *)
let rec encrypt_cesar k m b = match m with
    [] -> []
  | e::l -> (modulo (e+k) b)::encrypt_cesar k l b;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
 *)
let rec decrypt_cesar k m b = match m with
    [] -> []
  | e::l -> (modulo (e-k) b)::decrypt_cesar k l b;;

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)


let integer p q =
  let z = (p-1)*(q-1) in
  let rec integer2 p q z d = match d with
    |d when d > z -> invalid_arg"no number found"
    |d when d > 1 && d < z && gcd z d = 1 -> d
    |d -> integer2 p q z (d+1);
  in integer2 p q z 2;;

let inv a b =
  let rec bezout2  a b (d,u,v,d',u',v') = match d' with
      0 -> if u < 0 then u + b else u
    |d' ->  let q = d/d' in bezout2  a b (d',u',v',d-q*d',u-q*u',v-q*v');
  in bezout2  a b (a,1,0,b,0,1);;






let generate_keys_rsa p q =
  if p = q then invalid_arg "p and q must be distinct"
    else
  let n = p*q
  and z =(p-1)*(q-1) in
  let e = integer p q in
  let d = inv e z in
  (n,e),(n,d);;



(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) = mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) = mod_power m d n;;


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let g = quot (p-1) 2 in
  (g, p);;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let a = Random.int(p-2)+2 in
  let b = prime_mod_power g a p in
  (b, a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let k = Random.int(p-2)+2 in
  let x1 = prime_mod_power g k p in
  let x2 = modulo (msg * (prime_mod_power kA k p)) p in
  (x1, x2);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let (x, _, _) = bezout (prime_mod_power msgA a p) p in
  modulo (x * msgB) p;;
