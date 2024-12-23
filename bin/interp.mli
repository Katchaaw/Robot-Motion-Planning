val program1 : Pf5.Interp.instruction list
val program2 : Pf5.Interp.instruction list
val program3 : Pf5.Interp.instruction list
type color = { r : int; g : int; b : int; }
val color_to_graphics : color -> int
type options = {
  abs_rectangle : Pf5.Geo.rectangle option;
  show_points : bool;
  background_color : color option;
  foreground_color : color option;
  rectangle_color : color option;
  point_color : color option;
  window_size : (int * int) option;
  start_point : Pf5.Geo.coord2D option;
}
val parse_args : string list -> options
val apply_colors : options -> unit
val take : int -> 'a list -> 'a list
val calculate_steps : Pf5.Interp.program -> options -> Pf5.Geo.coord2D list
val display_cumulative_steps : options -> Pf5.Geo.coord2D list -> int -> unit
val run_interpreter : options -> Pf5.Interp.program -> unit
val main : string list -> unit
