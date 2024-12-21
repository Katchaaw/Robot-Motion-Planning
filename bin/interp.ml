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
  window_size: (int * int) option;
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
    | "-size" :: w :: h :: rest ->
        let width = int_of_string w in
        let height = int_of_string h in
        parse { opts with window_size = Some (width, height) } rest

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

  
let run_interpreter opts prog =
  (* Initialiser la fenêtre graphique*)
  Printf.printf "Initialisation de la fenêtre graphique...\n";
  Graphics.open_graph " 100x100 ";
  (*
  match opts.window_size with
    (* Taille donnée par l'utilisateur dans l'option -size *)
    | Some (w, h) -> 
        Graphics.open_graph (Printf.sprintf " %dx%d " w h);
        Printf.printf "Ouverture de la fenêtre avec taille %dx%d\n" w h;
    (* Taille par défaut *)
    | None -> 
        Printf.printf "Ouverture de la fenêtre avec taille par défaut 600x600\n";
        Graphics.open_graph " 600x600"; *)
      
  
  (* Appliquer les couleurs *)
  apply_colors opts;

  
  (* Vérifier l'option de rectangle abs *)
  (match opts.abs_rectangle with
    | Some rect -> 
      (* Vérifier que le rectangle contient l'origine (0, 0) *)
      if not (in_rectangle rect { x = 0.0; y = 0.0 }) then
        raise (Failure "L'origine (0, 0) n'est pas dans le rectangle spécifié")
      else
        (* Afficher le rectangle *)
        let corners = corners rect in
        Graphics.set_color (color_to_graphics (Option.get opts.rectangle_color));
        List.iter (fun p -> Graphics.fill_rect (int_of_float p.x) (int_of_float p.y) 3 3) corners
    | None -> ());

  (* Initialiser la position du robot à (0, 0) *)
  let robot_position = { x = 0.0; y = 0.0 } in
  let robot_position = ref robot_position 

  in

  
  (* Fonction pour afficher un point *)
  let display_point () =
    if opts.show_points then
      let point_color = 
        match opts.point_color with
        | Some color -> color_to_graphics color
        | None -> Graphics.red
      in
      Graphics.set_color point_color;
      Graphics.fill_circle (int_of_float !robot_position.x) (int_of_float !robot_position.y) 3
  in

  (* Fonction pour exécuter un mouvement de translation *)
  let execute_move trans =
    match trans with
    | Translate vector -> 
        (* Avant de déplacer le robot, tracer une ligne vers la nouvelle position *)
        let prev_pos = !robot_position in
        robot_position := translate vector !robot_position;
        Graphics.set_color (color_to_graphics (Option.get opts.foreground_color)); (* Couleur du trait *)
        Graphics.moveto (int_of_float prev_pos.x) (int_of_float prev_pos.y);
        Graphics.lineto (int_of_float !robot_position.x) (int_of_float !robot_position.y);
        display_point ();
        if opts.print_steps then 
          Unix.sleepf 0.5  (* Pause de 0.5 seconde entre chaque mouvement *)
    | Rotate (center, angle) -> 
        robot_position := rotate !robot_position angle center;
        display_point ();
        if opts.print_steps then 
          Unix.sleepf 2.0  (* Pause de 0.5 seconde entre chaque mouvement *)
  
  in

  
  (* Exécuter le programme pas à pas *)
  let rec execute_program program =
    match program with
    | [] -> ()
    | Move trans :: rest -> 
        execute_move trans;
        execute_program rest
    | Repeat (n, sub_program) :: rest -> 
        for _ = 1 to n do
          execute_program sub_program
        done;
        execute_program rest
    | Either (prog1, prog2) :: rest ->
        (* Choisir aléatoirement entre prog1 et prog2 *)
        let chosen_prog = if Random.int 2 = 1 then prog1 else prog2 in
        execute_program chosen_prog;
        execute_program rest
  
  in


  (* Exécuter le programme *)
  execute_program prog;
  
  (* Terminer l'exécution en maintenant la fenêtre ouverte *)
  ignore(Graphics.read_key ());
  Graphics.close_graph ()

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
