open WorldObject
open WorldObjectI

(* ### Part 3 Actions ### *)
let next_pollen_id = ref 0
let get_next_pollen_id () =
  let p = !next_pollen_id in incr next_pollen_id ; p

(* ### Part 3 Actions ### *)
let max_pollen = 5
let produce_pollen_probability = 50
let bloom_probability = 4000
let forfeit_pollen_probability = 3

(* ### Part 4 Aging ### *)
let flower_lifetime = 2000

(** Flowers produce pollen.  They will also eventually die if they are not cross
    pollenated. *)
class flower p pollen_id : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 3 Actions ### *)
  val mutable pollen_to_offer = World.rand max_pollen

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 3 Actions ### *)
  initializer
    self#register_handler World.action_event (fun _ -> self#do_action)

  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO: Part 3 Actions ### *)
  method private do_action : unit =
    if Random.float 1. < 1. /. (float produce_pollen_probability) then
      if pollen_to_offer < max_pollen then pollen_to_offer <- pollen_to_offer + 1;
    if Random.float 1. < 1. /. (float bloom_probability) then
      World.spawn 1 self#get_pos (fun p -> ignore (new flower p pollen_id))

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 1 Basic ### *)
  method get_name = "flower"

  (* ### TODO: Part 4 Aging ### *)
  method draw = self#draw_circle (Graphics.rgb 255 150 255) Graphics.black
                  (string_of_int pollen_to_offer)

  method draw_z_axis = 1

  (* ### TODO: Part 3 Actions ### *)
  method smells_like_pollen : int option =
    if pollen_to_offer > 0 then Some pollen_id
    else None

  method forfeit_pollen : int option =
    if pollen_to_offer > 0 then
      (if Random.float 1. < 1. /. (float forfeit_pollen_probability) then
         pollen_to_offer <- pollen_to_offer - 1;
         Some pollen_id)
    else None



  (***************************)
  (***** Ageable Methods *****)
  (***************************)

  (* ### TODO: Part 4 Aging ### *)

end
