open Pf5.Geo 
open Pf5.Interp 
(* open Pf5.Approx *)


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
    Move (Translate { x = 10.0; y = 0.0 }); (* Avance de 10 unités à droite *)
    Move (Translate { x = 0.0; y = 10.0 }); (* Avance de 10 unités vers le haut *)
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

let program4 = [
  Repeat (100, [
    Either (
      [Move(Translate {x = 1.; y = 0.})],
      [Move(Translate {x = -1.; y = 0.})]
    );
    Either (
      [Move(Translate {x = 0.; y = 1.})],
      [Move(Translate {x = 0.; y = -1.})]
    )
  ])
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
  window_size: (int * int) option;
  print_steps: bool;
  start_point: point option;  (* Nouveau champ pour le point de départ *)
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
    | "-size" :: w  :: h :: rest ->
        let width = int_of_string w in
        let height = int_of_string h in
        parse { opts with window_size = Some (width, height) } rest

    (* Gérer l'option -print pour afficher les étapes *)
    | "-print" :: rest ->
      parse { opts with print_steps = true } rest

    (* Gérer l'option -start pour le point de départ *)
    | "-start" :: x :: y :: rest ->
      let start = { x = float_of_string x; y = float_of_string y } in
      parse { opts with start_point = Some start } rest

    (* Terminer le parsing si aucune autre option n'est trouvée *)
    | [] -> opts

    | arg :: _ -> failwith (Printf.sprintf "Option inconnue : %s" arg)

  in
  parse { abs_rectangle = None; show_points = false; background_color = None;
          foreground_color = None; rectangle_color = None; point_color = None; window_size = None; print_steps = false; start_point = None} args

          
(* ############### Interpréteur ############### *)
(* Appliquer les couleurs et redessiner le rectangle selon les coordonnées centrées sur (0,0) *)
let apply_colors opts =
  (* Appliquer la couleur de l'arrière-plan *)
  (match opts.background_color with
   | Some color -> 
       Graphics.set_color (color_to_graphics color);
       Graphics.fill_rect 0 0 (Graphics.size_x ()) (Graphics.size_y ())
   | None -> ());

  (* Dessiner le rectangle si l'option -abs est spécifiée *)
  (match opts.abs_rectangle with
  | Some rect ->
    (* Appliquer la couleur du rectangle *)
    (match opts.rectangle_color with
    | Some color -> Graphics.set_color (color_to_graphics color)
    | None -> Graphics.set_color Graphics.black);

    (* Calculer les coordonnées du rectangle en pixels, centrées sur (0,0) *)
    let win_width = Graphics.size_x () in
    let win_height = Graphics.size_y () in
    let center_x = win_width / 2 in
    let center_y = win_height / 2 in

    let x_min = center_x + int_of_float (rect.x_min *. float_of_int win_width /. 200.0) in
    let y_min = center_y + int_of_float (rect.y_min *. float_of_int win_height /. 200.0) in
    let x_max = center_x + int_of_float (rect.x_max *. float_of_int win_width /. 200.0) in
    let y_max = center_y + int_of_float (rect.y_max *. float_of_int win_height /. 200.0) in

    let width = x_max - x_min in
    let height = y_max - y_min in

    (* Dessiner le rectangle *)
    Graphics.fill_rect x_min y_min width height;
    Graphics.synchronize ()  (* Assurez-vous que le dessin est affiché *)
  | None -> ());

  (* Appliquer la couleur du premier plan *)
  (match opts.foreground_color with
   | Some color -> Graphics.set_color (color_to_graphics color)
   | None -> ());

  (* Appliquer la couleur du point, si nécessaire *)
  (match opts.point_color with
   | Some color -> Graphics.set_color (color_to_graphics color)
   | None -> ())

(* ########################################################################################## *)

(* Fonction pour afficher les approximations rectangulaires 
let calculate_approximation program start_rect =
  over_approximate program start_rect
*)

let rec take n lst =
  match (n, lst) with
  | 0, _ -> []
  | _, [] -> []
  | n, x :: xs -> x :: take (n - 1) xs

(* Ajout d'une fonction pour calculer toutes les étapes d'un programme *)
let calculate_steps prog opts =
  let start_pos = match opts.start_point with
    | Some p -> p
    | None -> { x = 0.0; y = 0.0 }  (* Point de départ par défaut *)
  in
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
      (* Répéter n fois les sous-programmes et accumuler les résultats *)
      let repeated_steps = 
        let rec repeat n current_pos =
          if n <= 0 then []
          else
            let sub_steps = aux current_pos sub_program in
            let last_pos = List.hd (List.rev sub_steps) in
            sub_steps @ repeat (n-1) last_pos
        in
        repeat n current_pos
      in
      repeated_steps @ aux (List.hd (List.rev repeated_steps)) rest

    | Either (prog1, prog2) :: rest ->
        (* Choisir aléatoirement entre prog1 et prog2 *)
        let chosen_prog = if Random.bool () then prog1 else prog2 in
        let sub_steps = aux current_pos chosen_prog in
        let last_pos = List.hd (List.rev sub_steps) in
        sub_steps @ aux last_pos rest
  in
  start_pos :: aux start_pos prog


let display_cumulative_steps opts steps current_index =
  (* Effacer la fenêtre *)
  Graphics.clear_graph ();

  apply_colors opts;

  (* Récupérer la taille de la fenêtre *)
  let win_width = Graphics.size_x () in
  let win_height = Graphics.size_y () in

  (* Calculer l'échelle en fonction de la taille de la fenêtre *)
  let scale_x = float_of_int win_width /. 200.0 in
  let scale_y = float_of_int win_height /. 200.0 in

  (* Dessiner l'axe des abscisses (x) et des ordonnées (y) au centre *)
  let center_x = win_width / 2 in
  let center_y = win_height / 2 in

  (* Dessiner l'axe des abscisses *)
  Graphics.set_color Graphics.black;
  Graphics.moveto 0 center_y;
  Graphics.lineto win_width center_y;

  (* Dessiner l'axe des ordonnées *)
  Graphics.moveto center_x 0;
  Graphics.lineto center_x win_height;

  (* Fonction récursive pour dessiner les graduations sur l'axe X *)
  let rec draw_x_graduations i =
    if i <= 10 && i >= -10 then begin
      let x = center_x + int_of_float (float_of_int i *. 10.0 *. scale_x) in
      Graphics.set_color Graphics.black;
      Graphics.moveto x (center_y - 5);
      Graphics.lineto x (center_y + 5);
      (* Ajouter les étiquettes tous les 5 unités *)
      if i mod 2 = 0 then
        Graphics.draw_string (string_of_int i);
      draw_x_graduations (i + 1)
    end
  in
  (* Appeler la fonction récursive pour l'axe X *)
  draw_x_graduations (-10);

  (* Fonction récursive pour dessiner les graduations sur l'axe Y *)
  let rec draw_y_graduations i =
    if i <= 10 && i >= -10 then begin
      let y = center_y + int_of_float (float_of_int i *. 10.0 *. scale_y) in
      Graphics.set_color Graphics.black;
      Graphics.moveto (center_x - 5) y;
      Graphics.lineto (center_x + 5) y;
      (* Ajouter les étiquettes tous les 5 unités *)
      if i mod 2 = 0 && i <> 0 then
        Graphics.draw_string (string_of_int i);
      draw_y_graduations (i + 1)
    end
  in
  (* Appeler la fonction récursive pour l'axe Y *)
  draw_y_graduations (-10);

  (* Dessiner le chemin cumulatif *)
  let rec draw_path = function
    | [] | [_] -> () (* Pas de chemin à dessiner pour 0 ou 1 point *)
    | pos1 :: pos2 :: rest ->
        Graphics.set_color (color_to_graphics (Option.get opts.foreground_color));
        Graphics.moveto
          (center_x + int_of_float (pos1.x *. scale_x))
          (center_y + int_of_float (pos1.y *. scale_y));
        Graphics.lineto
          (center_x + int_of_float (pos2.x *. scale_x))
          (center_y + int_of_float (pos2.y *. scale_y));
        draw_path (pos2 :: rest)
  in

  (* Dessiner le chemin *)
  let steps_to_draw = take (current_index + 1) steps in
  draw_path steps_to_draw;

  (* Affichage des points si demandé *)
  if opts.show_points then
    let point_color =
      match opts.point_color with
      | Some color -> color_to_graphics color
      | None -> Graphics.red
    in
    Graphics.set_color point_color;
    List.iter
      (fun pos ->
         Graphics.fill_circle
           (center_x + int_of_float (pos.x *. scale_x))
           (center_y + int_of_float (pos.y *. scale_y))
           3)
      steps_to_draw

(* Exécution avec chemin cumulatif *)
let run_interpreter opts prog =
  (* Vérifier la taille minimale de la fenêtre *)
  let width, height =
    match opts.window_size with
    | Some (w, h) ->
        if w < 500 || h < 500 then (
          Printf.printf
            "Attention : La taille spécifiée (%dx%d) est trop petite. La taille minimale (400x400) sera appliquée.\n%!"
            w h;
          max 500 w, max 500 h
        ) else w, h
    | None -> 1080, 720 (* Taille par défaut *)
  in
  Graphics.open_graph (Printf.sprintf " %dx%d" width height);
  
  (* Pré-calculer toutes les étapes *)
  let steps = calculate_steps prog opts in

  (* Initialiser l'état *)
  let current_step = ref 0 in
  let total_steps = List.length steps in

  (* Définir les échelles initiales et le facteur de zoom *)
  let scale_x = ref 1.0 in
  let scale_y = ref 1.0 in
  let zoom_factor = 1.2 in

  (* Fonction pour afficher les options clavier *)
  let display_options () =
    let options = [
      "N : ETAPE SUIVANTE";
      "P : ETAPE PRECEDENTE";
      "R : REDIMENSIONNEMENT";
      "I : ZOOM IN";
      "O : ZOOM OUT";
      "Q : QUITTER";
    ] in
    let x = Graphics.size_x () - 150 in
    let y_start = Graphics.size_y () - 20 in
    Graphics.set_color Graphics.black;
    List.iteri
      (fun i option ->
        Graphics.moveto x (y_start - (i * 20));
        Graphics.draw_string option)
      options
  in

  (* Fonction pour gérer l'affichage et la navigation *)
  let rec loop () =
    (* Effacer la fenêtre *)
    Graphics.clear_graph ();

    (* Afficher les étapes cumulatives jusqu'à l'étape actuelle *)
    display_cumulative_steps opts steps !current_step;

    (* Afficher les options clavier *)
    display_options ();

    (* Gérer les entrées utilisateur *)
    let key = Graphics.read_key () in
    match key with
    | 'n' when !current_step < total_steps - 1 -> (* Étape suivante *)
      incr current_step;
      loop ()

    | 'p' when !current_step > 0 -> (* Étape précédente *)
      decr current_step;
      loop ()

    | 'i' -> (* Zoomer *)
      scale_x := !scale_x *. zoom_factor;
      scale_y := !scale_y *. zoom_factor;
      loop ()  (* Redessiner avec la nouvelle échelle *)

    | 'o' -> (* Dézoomer *)
      scale_x := !scale_x /. zoom_factor;
      scale_y := !scale_y /. zoom_factor;
      loop ()  (* Redessiner avec la nouvelle échelle *)

    | 'r' -> loop ()    (* Force un redimensionnement*)

    | 'q' -> (* Quitter *)
      Graphics.close_graph ()

    | _ -> loop () (* Continuer *)
  in 
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
    | "4" -> program4
    |  _ -> failwith "Programme non spécifié"
  in
  if prog != program4 then run_interpreter opts prog 
  else run_interpreter opts prog 


let () =
  try
    main (List.tl (Array.to_list Sys.argv))
  with
  | ex -> Printf.printf "Erreur inattendue : %s\n%!" (Printexc.to_string ex)