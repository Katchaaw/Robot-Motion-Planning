open Geo
open Interp

(* Code de la Section 5 du projet. *)

let () = Random.self_init ()

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


(* run_rect : program -> rectangle -> rectangle list *)
(* Simule une exécution du programme à partir d'une approximation rectangulaire
   de la position initiale et renvoie la liste des approximations successives. *)
let run_rect (prog : program) (initial_rect : rectangle) : rectangle list =
  let rec execute (prog : program) (current_rect : rectangle) (visited : rectangle list) : rectangle list =
    match prog with
    | [] -> List.rev visited (* Fin du programme, renvoyer la liste des rectangles visités *)
    | instruction :: rest ->
      let (intermediate_rects, new_rect) =
        match instruction with
        | Move t ->
          let new_rect = transform_rect t current_rect in
          ([new_rect], new_rect)
        | Repeat (n, sub_prog) ->
          (* Répéter n fois l'exécution du sous-programme *)
          let rec repeat n acc_rect acc_rects =
            if n = 0 then (acc_rects, acc_rect)
            else
              let sub_rects = execute sub_prog acc_rect [] in
              let new_rect = List.hd sub_rects in
              repeat (n - 1) new_rect (sub_rects @ acc_rects)
          in
          repeat n current_rect []
        | Either (prog1, prog2) ->
          (* Choix aléatoire entre deux sous-programmes *)
          let chosen_prog = if Random.int 2 = 0 then prog1 else prog2 in
          let rects = execute chosen_prog current_rect [] in
          (rects, List.hd rects)
      in
      (* Ajout de l'impression ici pour déboguer *)
      (*Printf.printf "Rectangles visités jusqu'à présent : %d\n" (List.length (intermediate_rects @ visited));*)
      (*List.iter (fun r -> Printf.printf "Rectangle: %f, %f, %f, %f\n" r.x_min r.x_max r.y_min r.y_max) (intermediate_rects @ visited);*)
      execute rest new_rect (intermediate_rects @ visited)
  in
  execute prog initial_rect [initial_rect]


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
    | [] -> false (* Si aucun rectangle visité = echec*)
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
        | Move t ->
          let new_state = transform t current_state in
          ([new_state], new_state)
        | Repeat (n, sub_prog) ->
          (* Répéter n fois l'exécution du sous-programme *)
          let rec repeat n acc_state acc_states =
            if n = 0 then (acc_states, acc_state)
            else
              let sub_states = execute sub_prog acc_state [] in
              let new_state = List.hd sub_states in
              repeat (n - 1) new_state (sub_states @ acc_states)
          in
          repeat n current_state []
        | Either (prog1, prog2) ->
          (* Choix aléatoire entre deux sous-programmes *)
          let chosen_prog = if Random.int 2 = 0 then prog1 else prog2 in
          let states = execute chosen_prog current_state [] in
          (states, List.hd states)
      in
      execute rest new_state (intermediate_states @ visited)
  in
  execute prog initial_state [initial_state]



(* over_approximate : program -> rectangle -> rectangle *)
(* Sur-approximation des positions atteignables par le robot *)
let over_approximate (prog : program) (r : rectangle) : rectangle =
  let rec execute prog r =
    match prog with
    | [] -> [r]
    | Move t :: rest ->
        let new_r = transform_rect t r in
        new_r :: execute rest new_r
    | Repeat (n, sub_prog) :: rest ->
        let rec repeat n acc_rect =
          if n = 0 then [acc_rect]
          else
            let sub_rects = execute sub_prog acc_rect in
            let new_rect = List.hd sub_rects in
            repeat (n - 1) new_rect @ sub_rects
        in
        repeat n r
    | Either (prog1, prog2) :: rest ->
        (* Sur-approximation en prenant l'enveloppe des résultats des deux programmes *)
        let rects1 = execute prog1 r in
        let rects2 = execute prog2 r in
        let all_rects = rects1 @ rects2 in
        
        let min_x = List.fold_left (fun acc rect -> min acc rect.x_min) max_float all_rects in
        let max_x = List.fold_left (fun acc rect -> max acc rect.x_max) min_float all_rects in
        let min_y = List.fold_left (fun acc rect -> min acc rect.y_min) max_float all_rects in
        let max_y = List.fold_left (fun acc rect -> max acc rect.y_max) min_float all_rects in
        [{x_min = min_x; x_max = max_x; y_min = min_y; y_max = max_y}]
  in
  List.hd (execute prog r)


(* Vérifie si un rectangle r1 est inclus dans un rectangle r2 *)
let is_rectangle_included (r1 : rectangle) (r2 : rectangle) : bool =
  r1.x_min >= r2.x_min &&
  r1.x_max <= r2.x_max &&
  r1.y_min >= r2.y_min &&
  r1.y_max <= r2.y_max

(* Fonction principale *)
let feasible_target_reached (prog : program) (initial_rect : rectangle) (target_rect : rectangle) : bool =
  let final_approx = over_approximate prog initial_rect in
  is_rectangle_included final_approx target_rect
