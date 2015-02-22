(*****************************************************************************)
(*                           Part 1 - Warmup                                 *)
(*****************************************************************************)

open Core.Std

(*>* Problem 1.0 *>*)
module type MATH =
sig
    val pi : float
    val cos : float -> float
    val sin : float -> float
    val sum : float -> float -> float
    val max : float list -> float option
end

(* Math: contains functions to compute cos, sin, sum, and max *)
module Math: MATH =
struct
    let pi = acos (-1.)
    let cos = cos
    let sin = sin
    let sum = (+.)
    let max (x : float list) = 
      match x with 
      | [] -> None
      | _ -> Some (List.fold_right x ~f:(fun x m ->
                                           if x < m then m else x) 
                                     ~init: Float.min_value)
end

let _ =
    assert(Math.pi = (acos (-1.)));
    assert(Math.cos 0.7 = cos 0.7);
    assert(Math.sin 1.3 = sin 1.3);
    assert(Math.sum (-0.832) 1.765 = (-0.832) +. 1.765)
    assert(Math.max [] = None);
    assert(Math.max [0.8;1.75;2.348;-0.943;7.546;-1.74] = Some 7.546)


(*>* Problem 1.1 *>*)

(* LIST: contains functions for length, fold_right, and rev for lists *)
module type LIST =
sig
    val length : 'a list -> int
    val fold_right : 'a list -> f:('a -> 'b -> 'b) -> init:'b -> 'b
    val rev : 'a list -> 'a list
end

module MyList = (List : LIST);;

let _ =
    assert(MyList.length [1;2;3] = 3);
    assert(MyList.fold_right ~f:(+) ~init:0 [1;2;3] = 6);
    assert(MyList.rev [1;2;3] = [3;2;1])


(*>* Problem 1.2 *>*)
module Allison =
struct
    type house =
        Adams | Lowell | Quincy |
        Kirkland | Winthrop | Eliot |
        Leverett | Mather | Dunster |
        Pforzheimer | Cabot | Currier

    type info = {
        hometown : string;
        year : int;
        concentration : string;
        house : house
    }

    let hometown = "Riverside, CA"
    let year = 2015
    let concentration = "Computer Science"
    let house = Adams
    let fold = List.fold_left ~f:(+)

    let info = {
        hometown;
        year;
        concentration;
        house
    }

    let grade_assignment assignment =
      "Everyone gets a perfect score for pset " ^ String.strip assignment ^ "!"

    let favorite_function _ = failwith "I don't have a favorite function"
    let least_favorite_function = ( ** )

    let print_info () =
        let _ = print_string (
            info.hometown ^ "\n" ^
            string_of_int year ^ "\n" ^
            info.concentration ^ "\n") in
        match info.house with
        | Adams -> print_string "Adams!\n"
        | _ -> failwith "Do any other houses matter?"

end

module Ben =
struct
    type info = {
        hometown : string;
        house : string;
        year : int;
        concentration : string
    }

    let least_favorite_function = (land)

    let rec info = {
        hometown = hometown;
        house = house;
        year = 2015;
        concentration = "Computer Science"
    }
    and hometown = "Holland, Michigan"
    and house = "Dunster"

    let grade_assignment assignment =
      "Everyone gets a zero for pset " ^ string_of_int assignment ^ ">:-("

    let favorite_function x y =
        log x +. log y +. Int64.to_float 0x46524F535459L

    let print_info = fun _ ->
        print_string (info.house ^ "\n" ^ info.hometown ^ "\n" ^
            string_of_int info.year ^ "\n" ^ info.concentration)

    let f = (-)
    let fold = List.fold_right ~f

end


(* TF: contains specific information about a TF *)
module type TF =
sig
    type info
    val info : info
    val favorite_function : float -> float -> float 
    val hometown : string
    val fold : int list -> init:int -> int
    val print_info : unit -> unit
end

module TFBen = (Ben : TF)
module TFAllison = (Allison : TF)


(*>* Problem 1.3 *>*)

module Jefferson =
struct
    type info = {
        hometown : string;
        year : int;
        concentration : string;
        house : string
    }

    let hometown = "Canadaigua, NY"
    let year = 2016
    let concentration = "Computer Science"
    let house = "Lowell"

    let info = {
        hometown;
        year;
        concentration;
        house
    }

    let fold = List.fold_left ~f:(+)
    let favorite_function _ = failwith "I don't have a favorite function"

    let print_info = fun _ ->
        print_string (info.house ^ "\n" ^ info.hometown ^ "\n" ^
            string_of_int info.year ^ "\n" ^ info.concentration ^
            "Jefferson lovveeeesss lillies")
end

module TFJefferson = (Jefferson : TF)
