open WorldObject
open WorldObjectI
open Bee

(** Bouncy bees will travel in a straight line in a random direction until an
    obstacle or edge of the world is reached, at which point a new random
    direction will be chosen. *)
class bee_bouncy p hive : bee_t =
object (self)
  inherit bee p hive as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO: Part 5 Smart Bees *)
  val mutable direction = Some (Direction.ord (World.rand 8))

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Bees *)

  (***********************)
  (***** Bee Methods *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees *)
  method private next_direction_default =
    let rec get_next_direction () = 
      if World.can_move (Direction.move_point self#get_pos direction) then
        direction
      else
        (direction <- Some (Direction.ord (World.rand 8));
         get_next_direction ())
    in
    get_next_direction ()


end


