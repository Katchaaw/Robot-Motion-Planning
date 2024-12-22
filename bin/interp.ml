(*open Graphics*)
open Pf5.Geo 
open Pf5.Interp 
(*open Pf5.Approx*)

(*exception Quit;;*)

let () = Random.self_init ()
(* ###############  Programmes passables en arguments ###############*)


(* Exemple 1 : Déplacement simple en Carré *)
let program1 = [
  Move (Translate { x = 50.0; y = 0.0 }); (* Avance de 10 unités à droite *)
  Move (Translate { x = 0.0; y = 50.0 }); (* Avance de 10 unités vers le haut *)
  Move (Translate { x = -50.0; y = 0.0 }); (* Retourne à gauche de 10 unités *)
  Move (Translate { x = 0.0; y = -50.0 }); (* Retourne en bas de 10 unités *)
]

(* Exemple 2 : Déplacement avec une répétition d'une boucle *)
let program2 = [
  Repeat (4, [
    Move (Translate { x = 50.0; y = 0.0 }); (* Avance de 10 unités à droite *)
    Move (Translate { x = 0.0; y = 50.0 }); (* Avance de 10 unités vers le haut *)
  ])
]

(* Exemple 3 : Soit un chemin carré, soit un autre chemin en diagonale *)
let program3 = [
  Either (
    [ 
      Move (Translate { x = 50.0; y = 0.0 }); (* Avance de 10 unités à droite *)
      Move (Translate { x = 0.0; y = 50.0 }); (* Avance de 10 unités vers le haut *)
      Move (Translate { x = -50.0; y = 0.0 }); (* Retourne à gauche de 10 unités *)
      Move (Translate { x = 0.0; y = -50.0 }); (* Retourne en bas de 10 unités *)
    ],
    [ 
      Move (Translate { x = 50.0; y = 50.0 }); (* Avance en diagonale droite-haut *)
      Move (Translate { x = -50.0; y = -50.0 }); (* Retourne en diagonale gauche-bas *)
    ]
  )
]


(* ############### Gestion des options ############### *)

(* Type représentant les couleurs *)
type color = { r: int; g: int; b: int }

(* Formatage du type couleur au type Graphics.rgb*)
let color_to_graphics {r; g; b} =
  Graphics.rgb r g b

(* Options de la ligne de commande *)
type options = {
  abs_rectangle: rectangle option;
  show_points: bool;
  background_color: color option;
  foreground_color: color option;
  rectangle_color: color option;
  point_color: color option;
  window_size: string option;
  print_steps: bool;
}

(* Fonction qui analyse les arguments passés en ligne de commande et configure les options *)
let parse_args args =
  Printf.printf "Arguments reçus : %s\n" (String.concat " " args);
  let rec parse opts = function
    (* Gérer l'option -abs *)
    | "-abs" :: x_min :: y_min :: x_max :: y_max :: rest ->
      let rect = { x_min = float_of_string x_min; y_min = float_of_string y_min; 
                  x_max = float_of_string x_max; y_max = float_of_string y_max } in
      parse { opts with abs_rectangle = Some rect } rest

    (* Gérer l'option -cr *)
    | "-cr" :: rest ->
      parse { opts with show_points = true } rest
  
    (* Gérer l'option -bc pour l'arrière-plan *)
    | "-bc" :: r :: g :: b :: rest ->
        let bg_color = { r = int_of_string r; g = int_of_string g; b = int_of_string b } in
        parse { opts with background_color = Some bg_color } rest

    (* Gérer l'option -fc pour le premier plan *)
    | "-fc" :: r :: g :: b :: rest ->
        let fg_color = { r = int_of_string r; g = int_of_string g; b = int_of_string b } in
        parse { opts with foreground_color = Some fg_color } rest

    (* Gérer l'option -rc pour le rectangle *)
    | "-rc" :: r :: g :: b :: rest ->
        let rect_color = { r = int_of_string r; g = int_of_string g; b = int_of_string b } in
        parse { opts with rectangle_color = Some rect_color } rest

    (* Gérer l'option -pc pour le point *)
    | "-pc" :: r :: g :: b :: rest ->
        let point_color = { r = int_of_string r; g = int_of_string g; b = int_of_string b } in
        parse { opts with point_color = Some point_color } rest
  
    (* Gérer l'option -size pour la taille de la fenêtre *)
    | "-size" :: w  :: rest ->
        parse { opts with window_size = Some (w) } rest

    (* Gérer l'option -print pour afficher les étapes *)
    | "-print" :: rest ->
      parse { opts with print_steps = true } rest

    (* Terminer le parsing si aucune autre option n'est trouvée *)
    | [] -> opts

    | arg :: _ -> failwith (Printf.sprintf "Option inconnue : %s" arg)
  in
  parse { abs_rectangle = None; show_points = false; background_color = None;
          foreground_color = None; rectangle_color = None; point_color = None; window_size = None; print_steps = false} args


(* ############### Interpréteur ############### *)

let apply_colors opts =

  (* Dessiner le rectangle si l'option -abs est spécifiée *)
  match opts.abs_rectangle with
  | Some rect ->
      (* Dessiner un rectangle avec les coordonnées définies dans -abs *)
      Graphics.set_color (color_to_graphics (Option.get opts.rectangle_color));
      Graphics.draw_rect
        (int_of_float rect.x_min) (int_of_float rect.y_min)
        (int_of_float (rect.x_max -. rect.x_min)) (int_of_float (rect.y_max -. rect.y_min))
  | None -> ();

  (* Appliquer la couleur de l'arrière-plan *)
  (match opts.background_color with
   | Some color -> 
       Graphics.set_color (color_to_graphics color);
       (* Remplir toute la fenêtre avec la couleur de fond *)
       Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ())
   | None -> ());
  
  (* Appliquer la couleur du premier plan *)
  (match opts.foreground_color with
   | Some color -> Graphics.set_color (color_to_graphics color)
   | None -> ());
  
  (* Appliquer la couleur du rectangle, si nécessaire *)
  (match opts.rectangle_color with
   | Some color -> Graphics.set_color (color_to_graphics color)
   | None -> ());
  
  (* Appliquer la couleur du point, si nécessaire *)
  (match opts.point_color with
   | Some color -> Graphics.set_color (color_to_graphics color)
   | None -> ())




(* ########################################################################################## *)

let rec take n lst =
  match (n, lst) with
  | 0, _ -> []
  | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs



(* Ajout d'une fonction pour calculer toutes les étapes d'un programme *)
let calculate_steps prog =
  let rec aux current_pos program =
    match program with
    | [] -> []
    | Move (Translate vector) :: rest ->
        let new_pos = translate vector current_pos in
        new_pos :: aux new_pos rest
    | Move (Rotate (center, angle)) :: rest ->
        let new_pos = rotate current_pos angle center in
        new_pos :: aux new_pos rest
    | Repeat (n, sub_program) :: rest ->
        let repeated_steps = List.init n (fun _ -> aux current_pos sub_program) |> List.flatten in
        repeated_steps @ aux (List.hd (List.rev repeated_steps)) rest
    | Either (prog1, prog2) :: rest ->
        let chosen_prog = if Random.int 2 = 1 then prog1 else prog2 in
        aux current_pos chosen_prog @ aux (List.hd (List.rev (aux current_pos chosen_prog))) rest
  in
  aux { x = 0.0; y = 0.0 } prog




(* Fonction pour afficher le chemin jusqu'à l'étape actuelle *)
let display_cumulative_steps opts steps current_index =
  (* Effacer la fenêtre *)
  Graphics.clear_graph ();

  (* Redessiner l'arrière-plan si nécessaire *)
  (match opts.background_color with
   | Some color ->
       Graphics.set_color (color_to_graphics color);
       Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ())
   | None -> ());

  (* Dessiner le chemin cumulatif *)
  let rec draw_path = function
  | [] | [_] -> () (* Pas de chemin à dessiner pour 0 ou 1 point *)
  | pos1 :: pos2 :: rest ->
      (* Dessiner une ligne entre deux points consécutifs *)
      Graphics.set_color (color_to_graphics (Option.get opts.foreground_color));
      Graphics.moveto (int_of_float pos1.x) (int_of_float pos1.y);
      Graphics.lineto (int_of_float pos2.x) (int_of_float pos2.y);
      draw_path (pos2 :: rest)
  in

  (* Commencer à dessiner depuis la position initiale (0,0) *)
  let steps_to_draw = take (current_index + 2) ({x = 0.0; y = 0.0} :: steps) in
  draw_path steps_to_draw;

  (* Toujours dessiner le point initial (0,0) si les points doivent être affichés *)
  if opts.show_points then
    let point_color =
      match opts.point_color with
      | Some color -> color_to_graphics color
      | None -> Graphics.red
    in
    Graphics.set_color point_color;
    Graphics.fill_circle 0 0 3; (* Affichage du point (0,0) *)

  (* Dessiner les points des étapes précédentes *)
  if opts.show_points then
    let rec draw_all_points steps idx =
      match steps with
      | [] -> ()
      | step :: rest when idx <= current_index ->
          let point_color =
            match opts.point_color with
            | Some color -> color_to_graphics color
            | None -> Graphics.red
          in
          Graphics.set_color point_color;
          Graphics.fill_circle (int_of_float step.x) (int_of_float step.y) 3;
          draw_all_points rest (idx + 1)
      | _ -> ()
    in
    draw_all_points steps 0


(* Exécution avec chemin cumulatif *)
let run_interpreter opts prog =
  (* Pré-calculer toutes les étapes *)
  let steps = calculate_steps prog in

  (* Initialiser l'état *)
  let current_step = ref 0 in
  let total_steps = List.length steps in

  (* Fonction pour gérer l'affichage et la navigation *)
  let rec loop () =
    (* Afficher les étapes cumulatives jusqu'à l'étape actuelle *)
    display_cumulative_steps opts steps !current_step;

    (* Gérer les entrées utilisateur *)
    let key = Graphics.read_key () in
    match key with
    | 'n' when !current_step < total_steps - 1 -> (* Étape suivante *)
        incr current_step;
        loop ()
    | 'p' when !current_step > 0 -> (* Étape précédente *)
        decr current_step;
        loop ()
    | 'q' -> (* Quitter *)
        Graphics.close_graph ()
    | _ -> loop () (* Continuer *)
  in

  (* Initialiser la fenêtre graphique 
  match opts.window_size with
  | Some size_str -> 
    Graphics.open_graph size_str  (* Utilisation de la chaîne de taille directement *)
  | None -> 
    Graphics.open_graph "600x600";  (* Taille par défaut si aucune taille n'est spécifiée *)
  *)
  
  Graphics.open_graph " 600x600 ";
  apply_colors opts;
  loop ()





let main args =
  (* Extraire le dernier argument comme identifiant de programme *)
  let (options, prog) =
    match List.rev args with
    | prog :: rest -> (List.rev rest, prog) (* Dernier argument = prog *)
    | [] -> failwith "Aucun argument fourni"
  in
  (* Analyser les options *)
  let opts = parse_args options in
  let prog = match prog with
    | "1" -> program1
    | "2" -> program2
    | "3" -> program3
    |  _ -> failwith "Programme non spécifié"
  in
  run_interpreter opts prog

let () =
  try
    main (List.tl (Array.to_list Sys.argv))
  with
  | ex -> Printf.printf "Erreur inattendue : %s\n%!" (Printexc.to_string ex)