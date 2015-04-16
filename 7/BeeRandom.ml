open WorldObject
open WorldObjectI
open Bee

(** Random bees will move randomly. *)
class bee_random p hive : bee_t =
object (self)
  inherit bee p hive as super

  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO: Part 5 Smart Bees *)
  method get_name = "bee_random"

  (***********************)
  (***** Bee METHODS *****)
  (***********************)

  (* ### TODO: Part 5 Smart Bees *)
  method next_direction = Some (Direction.ord (World.rand 8))

end


