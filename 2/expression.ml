open Core.Std ;;
open Ast ;;
open ExpressionLibrary ;;

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x" *)
let rec contains_var (e:expression) : bool =
  match e with
  | Var -> true
  | Num _ -> false
  | Binop (_,exp1,exp2) -> contains_var exp1 || contains_var exp2
  | Unop (_,exp) -> contains_var exp
;;

assert (contains_var (parse "x") = true);;
assert (contains_var (parse "3") = false);;
assert (contains_var (parse "4+3") = false);;
assert (contains_var (parse "x+3") = true);;
assert (contains_var (parse "x+3") = true);;
assert (contains_var (parse "sin x") = true);;
assert (contains_var (parse "4^3") = false);;


(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Don't
 *            worry about handling 'divide by zero' errors. *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
  | Var -> x
  | Num c -> c
  | Binop (op,exp1,exp2) ->
    (match op with
     | Add -> (evaluate exp1 x) +. (evaluate exp2 x)
     | Sub -> (evaluate exp1 x) -. (evaluate exp2 x)
     | Mul -> (evaluate exp1 x) *. (evaluate exp2 x)
     | Div -> (evaluate exp1 x) /. (evaluate exp2 x)
     | Pow -> (evaluate exp1 x) ** (evaluate exp2 x))
  | Unop (op,exp) ->
    (match op with
     | Sin -> sin (evaluate exp x)
     | Cos -> cos (evaluate exp x)
     | Ln -> log (evaluate exp x)
     | Neg -> -. (evaluate exp x))
;;

assert (evaluate (parse "x") 2. = 2.);;
assert (evaluate (parse "5") 2. = 5.);;
assert (evaluate (parse "x+5") 2. = 7.);;
assert (evaluate (parse "5-x") 2. = 3.);;
assert (evaluate (parse "5*x") 2. = 10.);;
assert (evaluate (parse "5/x") 2. = 2.5);;
assert (evaluate (parse "x^5") 2. = 32.);;
assert (evaluate (parse "sin x") 2. = (sin 2.));;
assert (evaluate (parse "cos x") 2. = (cos 2.));;
assert (evaluate (parse "ln x") 2. = (log 2.));;
assert (evaluate (parse "~x") 2. = -.2.);;


(*>* Problem 2.3 *>*)

(* derivative: takes the derivative of an expression and returns
               it as an expression *)
let rec derivative (e:expression) : expression =
    match e with
    | Num _ -> Num 0.
    | Var -> Num 1.
    | Unop (u,e1) ->
        (match u with
        | Sin -> Binop(Mul,Unop(Cos,e1),derivative e1)
        | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
        | Ln -> Binop(Mul,Binop(Div,Num 1.,e1),derivative e1)
        | Neg -> Unop(Neg,derivative e1))
    | Binop (b,e1,e2) ->
        match b with
        | Add -> Binop(Add,derivative e1,derivative e2)
        | Sub -> Binop(Sub,derivative e1,derivative e2)
        | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                         Binop(Mul,derivative e1,e2))
        | Div -> Binop(Div,Binop(Sub,Binop(Mul,derivative e1,e2),
                                   Binop(Mul,e1,derivative e2)),
                         Binop(Pow,e2,Num 2.))
        | Pow ->
            if contains_var e2
            then Binop(Mul,Binop(Pow,e1,e2),
                         Binop(Add,Binop(Mul,derivative e2,Unop(Ln,e1)),
                                 Binop(Div,Binop(Mul,derivative e1,e2),e1)))
            else Binop(Mul,e2,Binop(Mul,derivative e1,
                                      Binop(Pow,e1,Binop(Sub,e2,Num 1.))))
;;

assert (derivative (parse "5") = parse "0");;
assert (derivative (parse "x") = parse "1");;
assert (derivative (parse "sin x") = parse "cos x * 1");;
assert (derivative (parse "cos x") = parse "~(sin x)*1");;
assert (derivative (parse "ln x") = parse "1/x*1");;
assert (derivative (parse "~x") = parse "~1");;
assert (derivative (parse "x + sin x") = parse "1 + cos x * 1");;
assert (derivative (parse "x - ln x") = parse "1 - 1/x*1");;
assert (derivative (parse "x * sin x") = parse "x * (cos x * 1) + 1 * sin x");;
assert (derivative (parse "ln x / x") = parse "((1/x*1*x)-(ln x * 1))/(x^2)");;
assert (derivative (parse "x ^ x") = parse "x ^ x * (1 * ln x + 1 * x / x)");;
assert (derivative (parse "x ^ 3") = parse "3 * (1 * x ^ (3 - 1))");;

let checkexp strs xval =
    print_string ("Checking expression: " ^ strs ^ "\n");
    let parsed = parse strs in (
        print_string "contains variable : ";
        print_string (string_of_bool (contains_var parsed));
        print_endline " ";
        print_string "Result of evaluation: ";
        print_float (evaluate parsed xval);
        print_endline " ";
        print_string "Result of derivative: ";
        print_endline " ";
        print_string (to_string (derivative parsed));
        print_endline " "
    )
;;


(*>* Problem 2.4 *>*)

(* find_zero: uses Newton's method to approximate the zeros of a 
              function, returns None if an approximation is not
              found within the time limit *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
    : float option =
  let result = evaluate e g in
  if result < epsilon && result > -. epsilon
    then Some g
  else if lim = 0
    then None
  else find_zero e (g -. result /. (evaluate (derivative e) g)) 
         epsilon (lim-1)
;;

assert (let e = parse "x^2-3*x+2" in
        let zero = find_zero e 1. 0.0001 5 in
        match zero with
        | None -> false
        | Some x ->
          let result = evaluate e x in
          result < 0.0001 && result > -0.0001);;
assert (let e = parse "x^2-3*x+2" in
        let zero = find_zero e 100. 0.0001 5 in
        zero = None);;
assert (let e = parse "3+5" in
        let zero = find_zero e 0. 0.0001 50 in
        zero = None);;
assert (let e = parse "x^2-3*x+2" in
        let zero = find_zero e 1.3 0.0001 5 in
        match zero with
        | None -> false
        | Some x ->
          let result = evaluate e x in
          result < 0.0001 && result > -0.0001);;


(*>* Problem 2.5 *>*)

let float_equality (f1 : float) (f2: float) : bool =
  f1 -. f2 < 0.0000001 && f2 -. f1 < 0.0000001
;;

(* is_linear: heuristically checks if an expression is a polynomial 
              of degree 1 by checking if the derivative is constant
              (and nonzero) by evaluating the derivative at various
              arbitrarily chosen values (random values could also
              be used to minimize probability of false positives)
              and checking if they are equal (will return false
              positives in very specific cases). An approximation
              method is used since determining the degree of a 
              polynomial is undecidable if sin, cos, etc. are 
              allowed to be used (see: Richardson's theorem) *)
let is_linear (e:expression) : bool =
  let deriv = derivative e in
  let c = evaluate deriv 0. in
  let slopes = List.map [1.;1.5;2.;5.;100.;-3.5;-7.] 
                        ~f:(fun (x : float) -> evaluate deriv x)
  in
  List.fold_right slopes ~f:(fun (x : float) (lin : bool) -> 
                               float_equality x c && lin)
                         ~init:true
  && not (c = 0.)
;;

assert ((is_linear (parse "3")) = false);;
assert ((is_linear (parse "x")) = true);;
assert ((is_linear (parse "5*x - 4 + 3.5*x + 8")) = true);;
assert ((is_linear (parse "x^2 + 3*x - 5 - x^2")) = true);;
assert ((is_linear (parse "sin (2*x) - 2 * sin x * cos x + 4*x")) = true);;
assert ((is_linear (parse "cos x")) = false);;
assert ((is_linear (parse "x^4")) = false);;
assert ((is_linear (parse "x^x")) = false);;

(* find_zero_exact: finds exact zeros of expressions if the 
                    expression is a polynomial of degree 1,
                    only works if there are only constants and
                    degree 1 variables (i.e. no sin, cos, ln,
                    degree > 1 variables even if they cancel
                    to 0) *)
let rec find_zero_exact (e:expression) : expression option =
  if is_linear e then
    let rec accum_coef (e:expression) (coef:expression * expression)
            : expression * expression =
      match e with
      | Num _ -> (Num 0.,e)
      | Var -> (Num 1.,Num 0.)
      | Unop (u,e1) ->
        (match u with
         | Sin -> failwith "Can't handle this case"
         | Cos -> failwith "Can't handle this case"
         | Ln -> failwith "Can't handle this case"
         | Neg -> let (a',b') = accum_coef e1 (Num 0.,Num 0.) in
                  (Unop(Neg,a'),Unop(Neg,b')))
      | Binop (b,e1,e2) ->
        let (a1,b1) = accum_coef e1 (Num 0.,Num 0.) in
        let (a2,b2) = accum_coef e2 (Num 0.,Num 0.) in
        match b with
        | Add -> (Binop(Add,a1,a2),Binop(Add,b1,b2))
        | Sub -> (Binop(Sub,a1,a2),Binop(Sub,b1,b2))
        | Mul -> (Binop(Add,Binop(Mul,a1,b2),Binop(Mul,a2,b1)),Binop(Mul,b1,b2))
        | Div -> if contains_var e2 then failwith "Can't handle this case"
                 else (Binop(Div,a1,b2),Binop(Div,b1,b2))
        | Pow -> if contains_var e1 || contains_var e2 then
                   failwith "Can't handle this case"
                 else (Num 0.,e)
    in
    let (a,b) = accum_coef e (Num 0., Num 0.) in
    Some (Unop(Neg,Binop(Div,b,a)))
  else None
;;

(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 : int = 180;;
