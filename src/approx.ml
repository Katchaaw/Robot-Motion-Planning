open Geo
open Interp

(* Code de la Section 5 du projet. *)

let () = Random.self_init ()

(* sample : rectangle -> point *)
(* Renvoie un point choisi aléatoirement à l'intérieur du rectangle passé en paramètre. *)
let sample (r : rectangle) : point =
  let x_rand = Random.float (r.x_max -. r.x_min) +. r.x_min in
  let y_rand = Random.float (r.y_max -. r.y_min) +. r.y_min in
  { x = x_rand; y = y_rand }


(* transform_rect : transformation -> rectangle -> rectangle *)
(* Renvoie l’image d’un rectangle par la transformation donnée en argument. *)
let transform_rect (t : transformation) (r : rectangle) : rectangle =
  match t with
  | Translate v ->
      (* Translation : On translate directement les bornes du rectangle *)
      {
        x_min = r.x_min +. v.x;
        x_max = r.x_max +. v.x;
        y_min = r.y_min +. v.y;
        y_max = r.y_max +. v.y;
      }

  | Rotate (center, angle) ->
      (* Rotation : On calcule les images des coins du rectangle *)
      let rotated_corners = List.map (rotate center angle) (corners r) in
      (* On trouve le plus petit rectangle contenant tous les points après rotation *)
      rectangle_of_list rotated_corners


(* run_rect : program -> rectangle -> rectangle list *)
(* Simule une exécution du programme à partir d'une approximation rectangulaire
   de la position initiale et renvoie la liste des approximations successives. *)
let run_rect (prog : program) (initial_rect : rectangle) : rectangle list =
  let rec execute (prog : program) (current_rect : rectangle) (visited : rectangle list) : rectangle list =
    match prog with
    | [] -> List.rev visited

    | instruction :: rest ->
      let (intermediate_rects, new_rect) =
        match instruction with

        (* Appliquer les translations / rotations *)
        | Move t ->
          let new_rect = transform_rect t current_rect in
          ([new_rect], new_rect)

        (* Répéter n fois les sous-programmes*)
        | Repeat (n, sub_prog) ->
          let rec repeat n acc_rect acc_rects =
            if n = 0 then (acc_rects, acc_rect)
            else
              let sub_rects = execute sub_prog acc_rect [] in
              let new_rect = List.hd sub_rects in
              repeat (n - 1) new_rect (sub_rects @ acc_rects)
          in
          repeat n current_rect []

        (* Choix aléatoire entre deux sous-programmes *)
        | Either (prog1, prog2) ->
          let chosen_prog = if Random.int 2 = 0 then prog1 else prog2 in
          let rects = execute chosen_prog current_rect [] in
          (rects, List.hd rects)
      in
      execute rest new_rect (intermediate_rects @ visited)
  in
  execute prog initial_rect [initial_rect]

(* inclusion : rectangle -> rectangle -> bool *)
(* Vérifie si le rectangle r est inclus dans le rectangle t. *)
let inclusion (r : rectangle) (t : rectangle) : bool =
  r.x_min >= t.x_min && r.x_max <= t.x_max &&
  r.y_min >= t.y_min && r.y_max <= t.y_max
  

(* target_reached_rect : program -> rectangle -> rectangle -> bool *)
(* Vérifie si pour toutes les exécutions possibles du programme, le rectangle final est inclus dans la zone cible *)
let target_reached_rect (prog : program) (initial_rect : rectangle) (target : rectangle) : bool =
  (* On récupère toutes les combinaisons possibles d'exécution du programme *)
  let all_programs = all_choices prog in

  (* On vérifie que pour chaque programme possible, le rectangle final est inclus dans la cible *)
  List.for_all (fun program ->
    (* On simule l'exécution du programme sur l'approximation initiale *)
    let visited_rects = run_rect program initial_rect in
    match List.rev visited_rects with
      | [] -> false 
      | final_rect :: _ -> inclusion final_rect target
  ) all_programs


(* run_polymorphe : (transformation -> 'a -> 'a) -> program -> 'a -> 'a list *)
(* Fonction polymorphe pour exécuter un programme sur un état quelconque. *)
let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (initial_state : 'a) : 'a list =
  let rec execute (prog : program) (current_state : 'a) (visited : 'a list) : 'a list =
    match prog with
    | [] -> List.rev visited (* Fin du programme, renvoyer les états visités *)
    
    | instruction :: rest ->
      let (intermediate_states, new_state) =
        match instruction with
        (* Appliquer les translations / rotations *)
        | Move t ->
          let new_state = transform t current_state in
          ([new_state], new_state)

        (* Répéter n fois l'exécution du sous-programme *)
        | Repeat (n, sub_prog) ->
          let rec repeat n acc_state acc_states =
            if n = 0 then (acc_states, acc_state)
            else
              let sub_states = execute sub_prog acc_state [] in
              let new_state = List.hd sub_states in
              repeat (n - 1) new_state (sub_states @ acc_states)
          in
          repeat n current_state []

        (* Choix aléatoire entre deux sous-programmes *)       
        | Either (prog1, prog2) ->
          let chosen_prog = if Random.int 2 = 0 then prog1 else prog2 in
          let states = execute chosen_prog current_state [] in
          (states, List.hd states)
      in
      execute rest new_state (intermediate_states @ visited)
  in
  execute prog initial_state [initial_state]


(* over_approximate : program -> rectangle -> rectangle *)
(* Fonction qui calcule la surapproximation d'un rectangle après l'exécution d'un programme. *)
let over_approximate (prog : program) (r : rectangle) : rectangle =
  let rec execute prog acc_rect =
    Printf.printf "Rectangle courant avant transformation : x_min = %.2f, x_max = %.2f, y_min = %.2f, y_max = %.2f\n" acc_rect.x_min acc_rect.x_max acc_rect.y_min acc_rect.y_max;
    
    match prog with
    | [] -> acc_rect

    (* Appliquer les translations / rotations *)
    | Move t :: rest ->
        let new_rect = transform_rect t acc_rect in
        execute rest new_rect

    (* Répéter n fois l'exécution du sous-programme *)
    | Repeat (n, sub_prog) :: rest ->
        let rec repeat n current_rect =
          if n = 0 then current_rect
          else repeat (n - 1) (execute sub_prog current_rect)
        in
        execute rest (repeat n acc_rect)
        
    (* Choix entre deux sous-programmes *)
    | Either (prog1, prog2) :: rest ->
        (* Exécuter les deux sous-programmes et calculer la surapproximation *)
        let rect1 = execute prog1 acc_rect in
        let rect2 = execute prog2 acc_rect in
        let result_rect = {
          x_min = min rect1.x_min rect2.x_min;
          x_max = max rect1.x_max rect2.x_max;
          y_min = min rect1.y_min rect2.y_min;
          y_max = max rect1.y_max rect2.y_max;
        } in
        Printf.printf "Surapproximation après Either : x_min = %.2f, x_max = %.2f, y_min = %.2f, y_max = %.2f\n"
          result_rect.x_min result_rect.x_max result_rect.y_min result_rect.y_max;
        execute rest result_rect
  in
  execute prog r


(* feasible_target_reached : program -> rectangle -> rectangle -> bool *)
(* Vérifie si la surapproximation des positions atteignables par le robot est incluse dans la zone cible *)
let feasible_target_reached (prog : program) (initial_rect : rectangle) (target : rectangle) : bool =
  let over_approx = over_approximate prog initial_rect in
  inclusion over_approx target

