(** Carbon based objects eventually die, and leave dust behind when they do. *)
class carbon_based p inv_speed starting_lifetime max_lifetime : Ageable.ageable_t =
object (self)
  inherit Ageable.ageable p inv_speed starting_lifetime max_lifetime as super

  (***********************)
  (***** Initializer *****)
  (***********************)

  (* ### TODO: Part 4 Aging *)
  
  initializer
    self#register_handler self#get_die_event 
                          (fun _ -> ignore(new Dust.dust self#get_pos 
                                                         self#get_name));
  
end
