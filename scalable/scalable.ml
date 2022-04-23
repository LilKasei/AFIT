(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

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
(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  if n mod 2 = 0 then
    pow (pow x 2) (n/2)
  else
    x*(pow(pow x 2) ((n-1)/2));;
let abs x =
  
  if x > 0 then
    x
  else
    -x;;

let rec found num puiss =
  if power 2 puiss > num then
    puiss
  else
    found num (puiss+1);;


let en_bits integer bits =
  let rec conversion bits integer acu=
    if bits = -1 then
      acu
    else
      let pow = power 2 bits in
      if pow > integer then
         conversion (bits-1) integer (0::acu)
      else
         conversion (bits-1) (integer-pow) (1::acu)
  in conversion (bits-1) integer [];;

let from_int x =
  if x = 0 then
    []
  else
    let puiss = found (abs(x)) 0 in
    if x >= 0 then
      0:: en_bits x puiss
    else
      1 :: en_bits (abs(x)) puiss;;

    
    
   
        

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
*)
let list_to_dec list long =
  let rec resultat liste long =
      match liste with
          [] -> 0
        |(e::l) -> e* power 2 long + resultat l (long-1)
  in resultat list long;;

let to_int bA =
  let first_one liste =
    let rec destruc liste puis=
      match liste with
          [] -> 0
        |e::l-> e*power 2 puis  + destruc l (puis+1)
    in
    match liste with
      |[] -> 0
      |e::l when e =1 -> (-1)*destruc l 0
      |_::l -> 1 * destruc l 0
  in first_one bA;;



(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec printer liste =
    match liste with
      |[] -> print_string""
      |e::l -> print_int e;
        printer l
  in printer bA;;


(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
*)
let to_int_non_signe bA =
    let rec destruc liste puis=
      match liste with
          [] -> 0
        |e::l-> e*power 2 puis  + destruc l (puis+1)
  in destruc bA 0 ;;



let reverse bitarray =
  let rec rev bitarray acu =
      match bitarray with
      [] -> acu
     |e::l-> rev l (e::acu)
  in rev bitarray [];;

let rec length bitarray =
  match bitarray with
      [] -> 0
     |e::l -> 1 + length l ;;
        


let compare_n nA nB =
  let lena = length nA in
  let lenb = length nB in
  if lena > lenb then
    1
  else
    if lenb > lena then
      -1
    else
      let a = reverse nA in
      let b = reverse nB in
      let rec test a b =
        match (a,b) with
           ([],[]) -> 0
          |(e::l,r::q) when e = r -> test l q
          |(e::l,r::q) when e = 1 -> 1
          |_ -> (-1)
      in test a b;;



(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)

let (>>!) nA nB =
  if compare_n nA nB = 1 then
    true
  else
    false;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB =
    if compare_n nA nB =(-1) then
    true
  else
    false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB =
  let boolean = compare_n nA nB in
  if boolean =1 || boolean =0 then
    true
  else
    false;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB =
    let boolean = compare_n nA nB in
  if boolean =(-1) || boolean =0 then
    true
  else
    false;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let compare_b bA bB =
  match (bA,bB) with
     ([],[]) -> 0
    |(e::l,[])  when e =1 -> -1
    |(e::l,[])  when e =0-> 1
    |([],r::q)  when r=0 -> -1
    |([],r::q)  when r=1-> 1
    |(e::l,r::q) when e = r-> if e = 0 then
                                 compare_n l q 
                              else
                                 compare_n l q * (-1)
    |(e::l,r::q) when e < r -> 1
    |_ -> -1;;
     
  

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB =
    if compare_b bA bB = (-1) then
    true
  else
    false;;
(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB =
  if compare_b bA bB = 1 then
    true
  else
    false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB =
    if compare_b bA bB = 1 then
      false
    else
      true;;


let (>>=) bA bB =
  if compare_b bA bB = (-1) then
    false
  else
    true;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
  match bA with
    |[] -> 1
    | e::l when e =0 -> 1
    |_ -> -1;;
      

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
  match bA with
      [] ->[]
    |e :: l -> 0 :: l;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = 0

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = 0

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)


let add_n nA nB =
  let rec ajout l1 l2 retenue =
    match (l1,l2,retenue) with
      |([],[],0) -> []
      |([],[],retenue) -> 1 :: []
      |(e::l,[],retenue) -> (match (e,retenue) with
          |(0,0) -> 0:: ajout l [] 0
          |(0,1) -> 1:: ajout l [] 0
          |(1,0) -> 1:: ajout l [] 0
          |(_,_) -> 0:: ajout l [] 1)
      |([],r::q,retenue) -> (match (r,retenue) with
          |(0,0) -> 0:: ajout [] q 0
          |(0,1) -> 1:: ajout [] q 0
          |(1,0) -> 1:: ajout [] q 0
          |(_,_) -> 0:: ajout [] q 1)
      |(e::l,r::q,retenue) -> (match (e,r,retenue) with
          |(0,0,0) -> 0 :: ajout l q 0
          |(0,0,1) -> 1 :: ajout l q 0
          |(0,1,0) -> 1 :: ajout l q 0
          |(0,1,1) -> 0 :: ajout l q 1
          |(1,0,0) -> 1 :: ajout l q 0
          |(1,0,1) -> 0 :: ajout l q 1
          |(1,1,0) -> 0 :: ajout l q 1
          |(_,_,_) -> 1 :: ajout l q 1)
  in ajout nA nB 0;;
 

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)





let rec sur_n_bits bitarray n =
  match (bitarray,n) with
    |([],0) -> []
    |([],n) -> 0:: sur_n_bits [] (n-1)
    |(e::l,n) -> e::sur_n_bits l n;;

     
  



let rec test_diff_n x y retenue = (*a >b*)
  match (x,y,retenue) with
    |([],[],true) -> [1] (*pas censer arriver*)
    |([],[],false)-> [] (*pas censer arriver*)
    |([],_,_) -> [](*n'est pas cense arriver*)
    |(e::l,[],false) ->(match e with
        |0 -> 0::test_diff_n l y false
        |_ -> 1::test_diff_n l y false)
    |(e::l,[],true)-> (match e with
        |0 -> 1:: test_diff_n l y true
        |_ -> 0:: test_diff_n l y false)
    |(e::l,r::q,retenue) -> (match (e,r,retenue) with
        |(0,0,false) -> 0 :: test_diff_n l q false
        |(0,0,true) -> 1 :: test_diff_n l q true
        |(0,1,false) -> 1 :: test_diff_n l q true
        |(0,1,true) -> 0 :: test_diff_n l q true
        |(1,0,false) -> 1 :: test_diff_n l q false
        |(1,0,true) -> 0 :: test_diff_n l q false
        |(1,1,false) -> 0 :: test_diff_n l q false
        |_ -> 1 :: test_diff_n l q true);; (*forcement (1,1,1)*) (*ne gere pas les cas b>a*)

let rec enleve_0 list =
  match list with
    |[] ->[]
    |e::l when e =0 -> enleve_0 l
    |a -> a;;
  
let rec diff_n nA nB =
  let b = compare_n nA nB in
  match b with
    |0-> []
    |_-> reverse (enleve_0 (reverse (test_diff_n nA nB false))) ;;




   



(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB =
  let sign_A = sign_b(bA) in
  let sign_B = sign_b(bB) in
  match (bA,bB,sign_A,sign_B) with
      |(bA,[],_,_) -> bA
      |([],bB,_,_) -> bB
      |(e::l,r::q,-1,-1) -> 1:: add_n l q
      |(e::l,r::q,1,1) -> 0:: add_n l q
      |(e::l,r::q,-1,1) -> if compare_n l q = 1 then
                              1 :: diff_n l q
                           else
                              if compare_n l q = -1 then
                                 0 :: diff_n q l
                              else
                                 []
      |(_::l,_::q,_,_) -> if compare_n l q = 1 then
                             0 :: diff_n l q
                          else
                              if compare_n l q = -1 then
                                1 :: diff_n q l
                              else
                                    [];;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)


let diff_b bA bB =
  let sign_A = sign_b(bA) in
  let sign_B = sign_b(bB) in
  match (bA,bB,sign_A,sign_B) with
      |(bA,[],_,_) -> bA
      |([],r::q,_,-1) -> 0 :: q
      |([],r::q,_,1) -> 1 ::q
      |(e::l,r::q,-1,-1) -> if compare_n l q = 1 then
                               1:: diff_n l q
                            else
                               if compare_n l q = -1 then
                                 0:: diff_n q l 
                               else
                                 []
      |(e::l,r::q,1,1) -> if compare_n l q = (-1) then
                                1:: diff_n q l 
                          else
                                if compare_n l q =1 then
                                   0:: diff_n l q 
                                else
                                   []
      |(e::l,r::q,-1,1) -> 1:: add_n l q
      |(_::l,_::q,_,_) -> 0:: add_n l q
      |(_,_,_,_) -> [] ;;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let shift bA d =
  let rec insert bA d =
    match (bA,d) with
      |(bA,0) -> bA
      |(bA,d) -> 0:: insert bA (d-1)
  in
  match bA with
    |[]-> insert  [] d
    |e::l -> e :: insert l d;;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)

let rec shift_mult bA d =
   match (bA,d) with
      |(bA,0) -> bA
      |(bA,d) -> 0:: shift_mult bA (d-1);;

let mult_b bA bB =
  let rec mult l q resultat acu=
    match (l,q) with
    |([],q) -> resultat
    |(n::m,q) when n = 1 -> mult m q (add_n (shift_mult q acu) resultat) (acu+1)
    |(_::m,q) -> mult m q resultat (acu+1)
  in
  match (bA,bB) with
    |([],[]) -> []
    |(_,[]) -> []
    |([],_) -> []
    |(e::l,r::q) -> (match (e,r) with
        |(0,0) -> 0:: mult l q [0] 0
        |(1,1) -> 0:: mult l q [0] 0
        |_ -> 1 :: mult l q [0] 0);;



let mult_n bA bB =
  let rec mult l q resultat acu=
    match (l,q) with
    |([],q) -> resultat
    |(n::m,q) when n = 1 -> mult m q (add_n (shift_mult q acu) resultat) (acu+1)
    |(_::m,q) -> mult m q resultat (acu+1)
  in mult bA bB [0] 0;;
(** Quotient of two bitarrays.    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)
let quot_b bA bB =
  let rec div bA bB resultat =
    if compare_n bA bB = (-1) then
      resultat
    else
      div (diff_b bA bB) bB (add_n resultat [0;1])
  in div bA bB [];;



let getPowerTwo a b =
  let rec compte a b count acu =
    if compare_b a b != -1 then
      compte a (shift_mult b 1) (count+1) ((b, shift_mult [0;1] count)::acu)
    else
      acu
  in compte a b 0 [] ;;





let getMyList retenue coupleList =
  let rec getOthers retenue couplList final =
      match couplList with
        |[] -> final
        |(e,r)::l when (((>>=) retenue e ) && (retenue!=[])) ->
            getOthers (diff_b retenue e) l (add_b r final);
        |_::l ->getOthers  retenue l final
  in getOthers retenue coupleList [] ;;

let div_b a b =
  let v = getPowerTwo a b in
  getMyList a v;;




(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB =
  let signa = sign_b bA in
  let signb = sign_b bB in
  let rec div lA lB  =
    if compare_n lA lB = (-1) then
       lA 
    else
      div (diff_n lA lB ) lB 
  in
  let rec div2 lA lB  =
    if (>>=) lA [] && (<<) lA lB  then
       lA 
    else
      div2 (add_b lA lB) lB 
  in
  match (bA,bB) with
    ([], _::_) -> []
    |(bA,[]) -> bA
    |(e::l,r::q)-> (match (signa,signb)with
            |(-1,1) -> div2 bA bB
            |_-> (match (div l q) with
                |[] ->[]
                |e -> 0:: e));;
  




(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB =
  let rec div bA bB resultat =
    if compare_n bA bB = (-1) then
      (bA,resultat)
    else
      div (diff_n bA bB ) bB (add_n resultat [1])
  in div bA bB [0;0];;

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
