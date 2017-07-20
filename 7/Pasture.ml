open WorldObject
open WorldObjectI

(* ### Part 6 Custom Events ### *)
let smelly_object_limit = 200

(** A pasture will spawn a cow when there are enough objects in the world that
    smell like pollen. *)
class pasture p hive : world_object_i =
object (self)
  inherit world_object p as super

  (******************************)
  (***** Instance Variables *****)
  (******************************)

  (* ### TODO Part 6 Custom Events ### *)

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO Part 6 Custom Events ### *)
  initializer
    self#register_handler World.action_event (fun _ -> self#do_action)


  (**************************)
  (***** Event Handlers *****)
  (**************************)

  (* ### TODO Part 6 Custom Events ### *)
  method private do_action : unit =
    if World.fold (fun x n -> match x#smells_like_pollen with
                              | None -> n
                              | Some _ -> n + 1) 0 > smelly_object_limit
       && not (World.fold (fun x b -> b || x#get_name = "cow") false)
    then
      (ignore(new Cow.cow self#get_pos hive self);
       print_string "mooooooooo ";
       flush_all ())


  (********************************)
  (***** WorldObjectI Methods *****)
  (********************************)

  (* ### TODO Part 1 Basic ### *)

  method get_name = "pasture"

  method draw = self#draw_circle (Graphics.rgb 70 100 130) Graphics.white "P"

  method draw_z_axis = 1


end

