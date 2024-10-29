open Geo
open Interp

(* Code de la Section 5 du projet. *)

(* sample : rectangle -> point *)
(* Renvoi un point choisi aléatoirement à l'intérieur du rectangle passé en paramètre. *)
let sample (r : rectangle) : point =
  let x_rand = Random.float (r.x_max -. r.x_min) +. r.x_min in
  let y_rand = Random.float (r.y_max -. r.y_min) +. r.y_min in
  { x = x_rand; y = y_rand }
  

(* Fonction pour transformer les coins d'un rectangle selon une transformation donnée *)
let transform_corners (t : transformation) (corners : point list) : point list =
  match t with
  | Translate v ->
      (* Appliquer la translation à chaque coin *)
      List.map (fun p -> translate v p) corners
  | Rotate (c, alpha) ->
      (* Appliquer la rotation à chaque coin *)
      List.map (fun p -> rotate c alpha p) corners


(* Fonction pour transformer un rectangle selon une transformation donnée *)
let transform_rect (t : transformation) (r : rectangle) : rectangle =
  (* Définir les coins du rectangle *)
  let corners = corners r in
  
  (* Transformer les coins du rectangle *)
  let new_corners = transform_corners t corners in

  (* Trouver le rectangle englobant des nouveaux coins *)
  rectangle_of_list new_corners


let run_rect (prog : program) (r : rectangle) : rectangle list =
  failwith "À compléter"

let inclusion (r : rectangle) (t : rectangle) : bool =
  r.x_min >= t.x_min && r.x_max <= t.x_max &&
  r.y_min >= t.y_min && r.y_max <= t.y_max

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"

let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  failwith "À compléter"

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  failwith "À compléter"

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
