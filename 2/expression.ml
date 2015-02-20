open Core.Std ;;
open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry
 *    about expressionLibrary.ml
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
    match e with
    |Var -> true
    |Num _ -> false
    |Binop (_, e2, e3) -> contains_var e2 || contains_var e3
    |Unop (_, e2) -> contains_var e2
;;

assert ((contains_var (parse "x^4")) = true) ;;
assert ((contains_var (parse "4+3")) = false) ;;
assert ((contains_var (parse "x")) = true) ;;
assert ((contains_var (parse "4*x")) = true) ;;

(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Don't
 *            worry about handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
    match e with 
    |Var -> x
    |Num y -> y
    |Binop (oper, e1, e2) ->
      (match oper with
        |Add -> evaluate e1 x +. evaluate e2 x
        |Sub -> evaluate e1 x -. evaluate e2 x
        |Mul -> evaluate e1 x *. evaluate e2 x
        |Div -> evaluate e1 x /. evaluate e2 x
        |Pow -> evaluate e1 x ** evaluate e2 x
      )
    |Unop (oper, e1) ->
      (match oper with 
        |Neg -> evaluate e1 x *. -1.
        |Sin -> sin (evaluate e1 x)
        |Cos -> cos (evaluate e1 x)
        |Ln -> log (evaluate e1 x)
      )     
;;

assert ((evaluate (parse "x^4 + 3") 2.0) = 19.0 ) ;;
assert ((evaluate (parse "x*3 - 1") 2.0) = 5.0 ) ;;
assert ((evaluate (parse "x/2") 4.0) = 2.0 ) ;;
assert ((evaluate (parse "sin x") 2.0) = (sin 2.)) ;;
assert ((evaluate (parse "cos x") 2.0) = (cos 2.)) ;;
assert ((evaluate (parse "ln x") 2.0) = (log 2.)) ;;
assert ((evaluate (parse "~x") 2.0) = -2.) ;;


(*>* Problem 2.3 *>*)

(* See writeup for instructions. *)
let rec derivative (e:expression) : expression =
    match e with
    | Num _ -> Num 0.
    | Var -> Num 1.
    | Unop (u,e1) ->
        (match u with
        | Sin -> Binop(Mul,Unop(Cos,e1),derivative e1)
        | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
        | Ln -> Binop(Div, derivative e1, e1)
        | Neg -> Unop(Neg,derivative e1))
    | Binop (b,e1,e2) ->
        (match b with
        | Add -> Binop(Add,derivative e1,derivative e2)
        | Sub -> Binop(Sub,derivative e1,derivative e2)
        | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                        Binop(Mul,derivative e1,e2))
        | Div -> Binop(Div, Binop(Sub, Binop(Mul,derivative e1,e2),
                                  Binop (Mul,derivative e2,e1)),
                       Binop(Pow,e2,Num 2.)) 
        | Pow ->
            if contains_var(e2) = false
            then Binop(Mul,Binop(Mul,derivative e1,e2),
                       Binop(Pow,e1,Binop(Sub,e2,Num 1.)))
            else Binop(Mul,Binop(Pow,e1,e2),
                       Binop(Add,Binop(Mul,derivative e2,Unop(Ln,e1)),
                             Binop(Div,Binop(Mul,derivative e1,e2),e1))))
;;

assert ((derivative (parse "x^2 + x")) = (parse "(1.*2.)*x^(2.-1.)+1."));;
assert ((derivative (parse "x/(x^2)")) = (parse "(1.*x^2.-((1.*2.)*x^(2.-1.))*x)/(x^2.)^2."));;
assert ((derivative (parse "x*x - 3*x")) = (parse "(x*1.+1.*x)-(3.*1.+0.*x)"));;
assert ((derivative (parse "sin x")) = (parse "cos(x)*1."));;
assert ((derivative (parse "cos x")) = (parse "~(sin(x))*1."));;
assert ((derivative (parse "~ln x")) = (parse "~(1./x)" ));;

(* A helpful function for testing. See the writeup. *)
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

(* See writeup for instructions. *)
let rec find_zero (e:expression) (g:float) (epsilon:float) (lim:int)
  : float option =
  let eval = evaluate e g in
    if eval = 0. then (Some g)
    else if Float.abs(eval) < epsilon then (Some g)
         else if lim > 0 then 
                 let new_var = evaluate (Binop(Sub, Var, 
                                              Binop(Div,e,
                                                    derivative e))) g in
                 find_zero e new_var epsilon (lim-1)
              else None
;;

assert ((find_zero (parse "x^2 -x") 1.5 0.5 10) = Some 1.125);;
assert ((find_zero (parse "~x^3 - 2") 1.5 0.5 1) = None)
assert ((find_zero (parse "~x^2 - 2") 1.5 0.5 10) = Some 1.5);;
assert ((find_zero (parse "sin x + cos x") 0. 10. 2) = Some 0.);;


(*>* Problem 2.5 *>*)

(* Extra Credit:
 * Just leave it unimplemented if you don't want to do it.
 * See writeup for instructions. *)
(*let rec find_zero_exact (e:expression) : expression option =
    failwith "Not implemented"
;;*)


(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 : int = 4;;
