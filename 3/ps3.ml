open Core.Std
exception ImplementMe

(**************************** Part 1: Bignums *******************************)
type bignum = {neg: bool; coeffs: int list};;
let base = 10;;

(* Please make sure you fully understand the representation invariant for
 * bignums, as documented in the problem set specification. *)

(*>* Problem 1.1 *>*)
let negate (b : bignum) : bignum =
  let {neg = n; coeffs = i} = b in 
  if i = []
    then {neg = n; coeffs = i}
  else
    {neg = not n; coeffs = i}
;;

(* Sample negate tests: more exhaustive testing for all other functions
 * is required. (An example of such testing is test_equals below).
 * We only allow the positive representation of 0 *)

(*Test cases for negate*)
assert(negate {neg = false; coeffs = []}  = {neg = false; coeffs = []});;
assert(negate {neg = true; coeffs = [1; 2]} = {neg = false; coeffs = [1; 2]});;
assert(negate {neg = false; coeffs = [1; 2]} = {neg = true; coeffs = [1; 2]});;

(*>* Problem 1.3.1 *>*)
let fromInt (n: int) : bignum =
  let rec get_digits (num : int) : int list = 
    if num = 0 then
      []
    else 
      abs (num mod base) :: get_digits (num/base)
  in
  {neg = n<0; coeffs = List.rev (get_digits n)}
;;

(*Test cases for fromInt*)
assert(fromInt 0 = {neg = false; coeffs = []});;
assert(fromInt 123 = {neg = false; coeffs = [1; 2; 3]});;
assert(fromInt 00123 = {neg = false; coeffs = [1; 2; 3]});;
assert(fromInt (-123) = {neg = true; coeffs = [1; 2; 3]});;
assert(fromInt (-12300) = {neg = true; coeffs = [1; 2; 3; 0; 0]});;

(** Some helpful functions **)

(* Removes zero coefficients from the beginning of the bignum representation *)
let rec stripzeroes (b : int list) : int list =
  match b with
    | 0 :: t -> stripzeroes t
    | _ -> b


(* stripzeroes a bignum *)
let clean (b : bignum) : bignum =
  {neg = b.neg; coeffs = stripzeroes b.coeffs}


(* Returns a random bignum from 0 to bound - 1 (inclusive).
 * Can use this to help randomly test functions. *)
let randbignum (bound : bignum) =
  let rec randbignum_rec (bound : int list) =
    match bound with
      | [] -> []
      | [h] -> if h = 0 then [] else [Random.int h]
      | _ :: t -> Random.int base :: randbignum_rec t
  in {neg = false; coeffs = List.rev (randbignum_rec (List.rev bound.coeffs))}


(** Some helpful string functions **)
(* Splits a string into a list of its characters. *)
let rec explode (s : string) : char list =
  let len = String.length s in
  if len = 0 then []
  else s.[0] :: explode (String.sub s ~pos:1 ~len:(len - 1))

(* Condenses a list of characters into a string. *)
let rec implode (cs : char list) : string =
  match cs with
    | [] -> ""
    | c :: t -> String.make 1 c ^ implode t

(** Other functions you may find useful. *)
(* Returns the first n elements of list l (or the whole list if too short) *)
let rec take_first (l : 'a list) (n : int) : 'a list =
  match l with
    | [] -> []
    | h :: t -> if n <= 0 then [] else h :: take_first t (n - 1)

(* Returns a pair
 * (first n elements of lst, rest of elements of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h :: t -> let (lst1, lst2) = split t (n - 1) in
                (h :: lst1, lst2)

(* Returns the floor of the base 10 log of an integer *)
let intlog (base : int) : int =
  Float.to_int (log10 (Float.of_int base))


(* fromString and toString assume the base is a power of 10 *)
(* Converts a string representing an integer to a bignum. *)
let fromString (s : string) : bignum =
  let rec fromString_rec (cs : char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog base) in
    let string_to_convert = implode (List.rev chars_to_convert) in
    int_of_string string_to_convert :: fromString_rec rest
  in
  match explode s with
    | [] -> fromInt 0
    | h :: t -> if h = '-' || h = '~' then
        {neg = true; coeffs = (List.rev (fromString_rec (List.rev t)))}
      else {neg = false;
            coeffs = stripzeroes (List.rev (fromString_rec (List.rev (h :: t))))}
;;

(* Converts a bignum to its string representation.
 * Returns a string beginning with ~ for negative integers. *)
let toString (b : bignum) : string =
  let rec pad_with_zeroes_left (s : string) (len : int) =
    if String.length s >= len then s else
      "0" ^ pad_with_zeroes_left s (len - 1) in
  let rec stripstrzeroes (s : string) (c : char) =
    if String.length s = 0 then
      "0"
    else if String.get s 0 = '0' then
      stripstrzeroes (String.sub s ~pos:1 ~len:(String.length s - 1)) c
    else s in
  let rec coeffs_to_string (coeffs : int list) : string =
    match coeffs with
      | [] -> ""
      | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog base)
                  ^ coeffs_to_string t in
  let stripped = stripzeroes b.coeffs in
  if List.length stripped = 0 then "0"
  else let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
       if b.neg then "~" ^ from_coeffs else from_coeffs


(*>* Problem 1.2 *>*)
let equal (b1 : bignum) (b2 : bignum) : bool =
  b1 = b2
;;

(* Automated testing function. Use this function to help you catch potential
 * edge cases. While this kind of automated testing is helpful, it is still
 * important for you to think about what cases may be difficult for your
 * algorithm. Also, think about what inputs to run the testing function on. If
 * you're having trouble isolating a bug, you can try printing out which values
 * cause an assert failure. *)

(*Determines if first bignum is equal to second bignum*)
let rec test_equal (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(equal (fromInt count) (fromInt max) = (count = max)) in
    test_equal (count + 1) max

(*Tests for equal*)
let () = test_equal (-10000) 10000
let () = test_equal 10000 (-10000)
let () = test_equal (-10000) 9999

(*Determines if first bignum is less than second bignum*)
let less (b1 : bignum) (b2 : bignum) : bool =
  let rec checkNums l1 l2 = 
    match (l1,l2) with
    |([],[]) -> true
    |(hd1::[], hd2::[]) -> hd1 < hd2
    |(hd1::tl1, hd2::tl2) -> if hd1 < hd2 
                                then true
                             else if hd1 > hd2
                                then false
                             else checkNums tl1 tl2
    |(_,_) -> false
  in                            
  let listCompare l1 l2 =
    if List.length l1 > List.length l2
      then false
    else if List.length l1 = List.length l2
      then checkNums l1 l2
    else true
  in
  let {neg = n1; coeffs = l1} = b1 in
  let {neg = n2; coeffs = l2} = b2 in
  if n1 && not n2
     then true
  else if n2 && not n1
     then false
  else 
    if n1 
       then not (listCompare l1 l2) 
    else listCompare l1 l2
;;

(*Test fucntion for less*)
let rec test_less (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(less (fromInt count) (fromInt max) = (count < max)) in
    test_less (count + 1) max
;;

(*Tests for less*)
let () = test_less (-10000) 10000;;
let () = test_less 10000 (-10000);;
let () = test_less (-10000) 9999;;


(*Determines if first bignum is greater than second bignum*)
let greater (b1 : bignum) (b2 : bignum) : bool =
  if equal b1 b2
    then false
  else if less b1 b2
    then false
  else true
;;

(*Test function for greater*)
let rec test_greater (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(greater (fromInt count) (fromInt max) = (count > max)) in
    test_greater (count + 1) max
;;

(*Tests for greater*)
let () = test_greater (-10000) 10000;;
let () = test_greater 10000 (-10000);;
let () = test_greater (-10000) 9999;;

(*>* Problem 1.3.2 *>*)

(*Changes bignum into int. 
  Will return None is bignum is bigger than int allows*)
let toInt (b : bignum) : int option =
  let rec from_digits (bList : int list) (pow : int)= 
    match bList with
    |[] -> Some 0
    |hd::[] -> Some (hd*pow)
    |hd::tl ->
      (match from_digits tl (pow*base) with 
       |Some num -> if (hd*pow) < hd then
                       None
                    else
                       Some (hd*pow + num)
       |None -> None)
  in
  let {neg = n; coeffs = i} = b in
  let revList = List.rev i in 
    match from_digits revList 1 with
    |Some c -> if n then 
                  Some (-c) 
               else Some c
    |None -> None
;;

(*Test function for toInt*)
let rec test_toInt (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(toInt (fromInt count)= (Some count)) in
    test_toInt (count + 1) max
;;


(*Tests for toInt*)
let () = test_toInt (-10000) 10000;;
assert(toInt {neg = false; coeffs = []} = Some 0);;
assert(toInt {neg = false; coeffs = [1; 2; 3]} = Some 123);;
assert(toInt {neg = true; coeffs = [1; 2; 3]} = Some (-123));;
assert(toInt {neg = true; coeffs = [1; 2; 3; 0; 0]} = Some (-12300));;
assert(toInt {neg=false; coeffs=[1;2;3;4;5;6;7;8;9;8;7;6;5;4;3;2;1]} = None);;

(** Some arithmetic functions **)

(* Returns a bignum representing b1 + b2.
 * Assumes that b1 + b2 > 0. *)
let plus_pos (b1 : bignum) (b2 : bignum) : bignum =
  let pair_from_carry (carry : int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry : int)
            : bool * int list =
    match (coeffs1, coeffs2) with
      | ([], []) -> pair_from_carry carry
      | ([], _) -> if carry = 0 then (neg2, coeffs2) else
          plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
      | (_, []) -> if carry = 0 then (neg1, coeffs1) else
          plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
      | (h1 :: t1, h2 :: t2) ->
          let (sign1, sign2) =
            ((if neg1 then -1 else 1), (if neg2 then -1 else 1)) in
          let result = h1 * sign1 + h2 * sign2 + carry in
          if result < 0 then
            let (negres, coeffsres) =
                  plus_with_carry (neg1, t1) (neg2, t2) (-1)
            in (negres, result + base :: coeffsres)
          else if result >= base then
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
            in (negres, result - base :: coeffsres)
          else
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
            in (negres, result :: coeffsres)
  in
  let (negres, coeffsres) =
        plus_with_carry (b1.neg, List.rev b1.coeffs)
                        (b2.neg, List.rev b2.coeffs)
                        0
  in {neg = negres; coeffs = stripzeroes (List.rev coeffsres)}
;;

(*>* Problem 1.4 *>*)
(* Returns a bignum representing b1 + b2.
 * Does not make the above assumption.
 * Hint: How can you use plus_pos to implement this?
*)
let plus (b1 : bignum) (b2 : bignum) : bignum =
  let {neg = n1; coeffs = l1} = b1 in 
  let {neg = n2; coeffs = l2} = b2 in
  if n1 = n2 then 
    let {neg=_; coeffs=lf} = plus_pos {neg=false; coeffs=l1} {neg=false; coeffs=l2} in
    {neg = n1; coeffs = lf}
  else if (n1 && greater (negate b1) b2) || (n2 && less b1 (negate b2))
     then negate (plus_pos (negate b1) (negate b2))
  else 
      plus_pos b1 b2
;;

(*Test function for plus*)
let rec test_plus (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(plus (fromInt count) (fromInt max)=(fromInt (count+max))) in
    test_plus (count + 1) max
;;

(*Tests for plus*)
let () = test_plus (-10000) 10000;;
let () = test_plus 10000 (-10000);;
let () = test_plus (-10000) 9999;;


(*>* Problem 1.5 *>*)
(* Returns a bignum representing b1*b2.
 * Things to think about:
 * 1. How did you learn to multiply in grade school?
 * (In case you were never taught to multiply and instead started with
 * set theory in third grade, here's an example):
 *
 *      543
 *     x 42
 *     ____
 *     1086
 *   +21720
 *   =22806
 *
 * 2. Can you use any functions you've already written?
 * 3. How can you break this problem into a few simpler, easier-to-implement
 * problems?
 * 4. If your code is buggy, test helper functions individually instead of
 * the entire set at once.
 * 5. Assuming positivity in some of your helper functions is okay to
 * simplify code, as long as you respect that invariant.
*)
let times (b1 : bignum) (b2 : bignum) : bignum =
  let rec singleMult blist mult carry =
    match blist with
    |[] -> if carry > 0 
              then [carry]
           else
              []
    |hd::tl -> let product = (hd * mult) + carry in 
              (product mod base)::(singleMult tl mult (product/base))
  in
  let rec append0 blist pow =
    if pow > 0 
       then 0::(append0 blist (pow - 1))
    else blist
  in
  let rec multLists list1 list2 pow =
    match list2 with
    |[] -> {neg = false; coeffs = []}
    |hd::tl -> plus 
       ({neg = false; coeffs = List.rev (append0 (singleMult list1 hd 0) pow)})
       (multLists list1 tl (pow + 1))
  in
  let {neg = n1; coeffs = l1} = b1 in
  let {neg = n2; coeffs = l2} = b2 in
  if n1 = n2
    then multLists (List.rev l1) (List.rev l2) 0
  else
    negate (multLists (List.rev l1) (List.rev l2) 0)
;;

(*Test function for times*)
let rec test_times (count : int) (max : int) : unit =
  if count > max then ()
  else
    let _ = assert(times (fromInt count) (fromInt max) = (fromInt (count * max))) in
    test_times (count + 1) max
;;

(*Tests for times*)
let () = test_times (-10000) 10000;;
let () = test_times 10000 (-10000);;
let () = test_times (-10000) 9999;;

(* Returns a bignum representing b/n, where n is an integer less than base *)
let divsing (b : int list) (n : int) : int list * int =
  let rec divsing_rec (b : int list) (r : int) : int list * int =
    match b with
      | [] -> [], r
      | h :: t ->
          let dividend = r * base + h in
          let quot = dividend / n in
          let (q, r) = divsing_rec t (dividend-quot * n) in
          (quot :: q, r) in
    match b with
      | [] -> [], 0
      | [a] -> [a / n], a mod n
      | h1 :: h2 :: t -> if h1 < n then divsing_rec (h1 * base + h2 ::t) 0
        else divsing_rec b 0


(* Returns a pair (floor of b1/b2, b1 mod b2), both bignums *)
let divmod (b1 : bignum) (b2 : bignum): bignum * bignum =
  let rec divmod_rec m n (psum : bignum) : bignum * bignum =
    if less m n then (psum, m) else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
        | [] -> failwith "Division by zero"
        | ns :: _ -> let (p, _) =
            if ns + 1 = base then
              (take_first mc (List.length mc - List.length nc), 0)
            else
              let den = ns + 1 in
              let num = take_first mc (List.length mc - List.length nc + 1)
              in divsing num den
          in
          let bp = clean {neg = false; coeffs = p} in
          let p2 = clean (if equal bp (fromInt 0) then fromInt 1 else bp) in
            divmod_rec (clean (plus m (negate (times n p2))))
                       (clean n)
                       (clean (plus psum p2))
  in
  divmod_rec (clean b1) (clean b2) (fromInt 0)


(**************************** Challenge 1: RSA ******************************)

(** Support code for RSA **)
(* Hint: each part of this problem can be implemented in approximately one
 * line of code. *)

(* Returns b to the power of e mod m *)
let rec expmod (b : bignum) (e : bignum) (m : bignum) : bignum =
  if equal e (fromInt 0) then fromInt 1
  else if equal e (fromInt 1) then
    snd (divmod (clean b) (clean m))
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = expmod (clean b) q (clean m) in
    let (_, x) = divmod (times (times res res) (expmod (clean b) r (clean m)))
                        (clean m) in
    {neg = x.neg; coeffs = stripzeroes x.coeffs}

(* Returns b to the power of e *)
let rec exponent (b : bignum) (e : bignum) : bignum =
  if equal (clean e) (fromInt 0) then fromInt 1
  else if equal (clean e) (fromInt 1) then clean b
  else
    let (q, r) = divmod (clean e) (fromInt 2) in
    let res = exponent (clean b) q in
    let exp = (times (times res res) (exponent (clean b) r))
    in {neg = exp.neg; coeffs = stripzeroes exp.coeffs}

(* Returns true if n is prime, false otherwise. *)
let isPrime (n : bignum) : bool =
  let rec miller_rabin (k : int) (d : bignum) (s : int) : bool =
    if k < 0 then true else
    let rec square (r : int) (x : bignum) =
      if r >= s then false else
      let x = expmod x (fromInt 2) n in

        if equal x (fromInt 1) then false
        else if equal x (plus n (fromInt (-1))) then miller_rabin (k-1) d s
        else square (r + 1) x
    in
    let a = plus (randbignum (plus n (fromInt (-4)))) (fromInt 2) in
    let x = expmod a d n in
      if equal x (fromInt 1) || equal x (plus n (fromInt (-1))) then
        miller_rabin (k - 1) d s
      else square 1 x
  in
    (* Factor powers of 2 to return (d, s) such that n=(2^s)*d *)
  let rec factor (n : bignum) (s : int) =
    let (q, r) = divmod n (fromInt 2) in
      if equal r (fromInt 0) then factor q (s + 1) else (n, s)
  in
  let (_, r) = divmod n (fromInt 2) in
    if equal r (fromInt 0) then false else
      let (d, s) = factor (plus n (fromInt (-1))) 0 in
        miller_rabin 20 d s

(* Returns (s, t, g) such that g is gcd(m, d) and s*m + t*d = g *)
let rec euclid (m : bignum) (d : bignum) : bignum * bignum * bignum =
  if equal d (fromInt 0) then (fromInt 1, fromInt 0, m)
  else
    let (q, r) = divmod m d in
    let (s, t, g) = euclid d r in
      (clean t, clean (plus s (negate (times q t))), clean g)


(* Generate a random prime number between min and max-1 (inclusive) *)
let rec generateRandomPrime (min : bignum) (max: bignum) : bignum =
  let rand = plus (randbignum (plus max (negate min))) min in
    if isPrime rand then rand else generateRandomPrime min max


(** Code for encrypting and decrypting messages using RSA **)

(* Generate a random RSA key pair, returned as (e, d, n).
 * p and q will be between 2^n and 2^(n+1).
 * Recall that (n, e) is the public key, and (n, d) is the private key. *)
let rec generateKeyPair (r : bignum) : bignum * bignum * bignum =
  let c1 = fromInt 1 in
  let c2 = fromInt 2 in
  let p = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let q = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
  let m = times (plus p (negate c1)) (plus q (negate c1)) in
  let rec selectPair () =
    let e = generateRandomPrime (exponent c2 r) (exponent c2 (plus r c1)) in
    let (_, d, g) = euclid m e in
    let d = if d.neg then plus d m else d in
      if equal g c1 then (clean e, clean d, clean (times p q))
      else selectPair ()
  in
    if equal p q then generateKeyPair r else selectPair ()


(*>* Problem 2.1 *>*)

(* To encrypt, pass in n e s. To decrypt, pass in n d s. *)
let encryptDecryptBignum (n : bignum) (e : bignum) (s : bignum) : bignum =
  expmod s e n
;;


(* Pack a list of chars as a list of bignums, with m chars to a bignum. *)
let rec charsToBignums (lst : char list) (m : int) : bignum list =
  let rec encchars lst =
    match lst with
      | [] -> (fromInt 0)
      | c :: t -> clean (plus (times (encchars t) (fromInt 256))
                              (fromInt (Char.to_int c)))
  in
    match lst with
      | [] -> []
      | _ -> let (enclist, rest) = split lst m in
             encchars enclist :: charsToBignums rest m


(* Unpack a list of bignums into chars (reverse of charsToBignums) *)
let rec bignumsToChars (lst : bignum list) : char list =
  let rec decbignum b =
    if equal b (fromInt 0) then []
    else let (q, r) = divmod b (fromInt 256) in
      match toInt r with
        | None -> failwith "bignumsToChars: representation invariant broken"
        | Some ir -> Char.of_int_exn ir :: decbignum q
  in
    match lst with
      | [] -> []
      | b :: t -> decbignum b @ bignumsToChars t


(* Return the number of bytes required to represent an RSA modulus. *)
let bytesInKey (n : bignum) =
  Float.to_int (Float.of_int (List.length (stripzeroes n.coeffs) - 1)
                *. log10 (Float.of_int base) /. (log10 2. *. 8.))


(* Encrypts or decrypts a list of bignums using RSA.
 * To encrypt, pass in n e lst.
 * To decrypt, pass in n d lst. *)
let rec encDecBignumList (n : bignum) (e : bignum) (lst : bignum list) =
  match lst with
    | [] -> []
    | h :: t -> encryptDecryptBignum n e h :: encDecBignumList n e t



(*>* Problem 2.2 *>*)
let encrypt (n : bignum) (e : bignum) (s : string) =
  encDecBignumList n e (charsToBignums (explode s) (bytesInKey n))
;;


(* Decrypt an encrypted message (list of bignums) to produce the
 * original string. *)
let decrypt (n : bignum) (d : bignum) (m : bignum list) =
  implode (bignumsToChars (encDecBignumList n d m))
;;

let (e, d, n) = generateKeyPair (fromInt 100);;

assert(encryptDecryptBignum n d (encryptDecryptBignum n e (fromInt 123456789))
                   = (fromInt 123456789));;

let message = "Hi, I'm a secret message. Hopefully this function works
 wooohoo RSA rocks";;
 
assert(decrypt n d (encrypt n e message) = message);;

(**************** Challenge 2: Faster Multiplication *********************)

(* Returns a bignum representing b1*b2 *)
(*let times_faster (b1 : bignum) (b2 : bignum) : bignum =
  raise ImplementMe*)


let minutes_spent = 600;;
