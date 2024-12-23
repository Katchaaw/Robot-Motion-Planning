open Geo
(* Code de la Section 4 du projet. *)

(*############## Définition des types ##############*)
type instruction =
  Move of transformation
| Repeat of int * program
| Either of program * program 
and program = instruction list


(*############ Début du code ##############*)

(* Exception levée pour signaler la présence d'un programme 
   non-déterministe dans un contexte déterministe *)
exception NonDeterministicProgram

(* Initialisation du module Random pour les choix aléatoires *)
let () = Random.self_init ()


(*is_deterministic : program -> bool*)
(* Vérifie si un programme est déterministe (ne contient pas d'instruction `Either`). *)
let rec is_deterministic (prog : program) : bool =
  (* On itère sur tous les éléments de prog *)
  List.for_all (fun instruction ->
    match instruction with
    | Move _ -> true
    (*On vérifie si les sous-programmes dans Repeat sont déterministes*)
    | Repeat (_, prog_sub) -> is_deterministic prog_sub
    | Either (_, _) -> false
  ) prog


(* unfold_repeat : program -> program*)
(* Développe les instructions `Repeat` en 
   dupliquant les sous-programmes qu'elles contiennent.*)
let rec unfold_repeat (prog : program) : program =
  (* Combine les listes de programmes de prog en une seule liste *)
  List.flatten (
    (* List.map applique la fonction à chaque élement de la liste*)
    List.map (fun instruction ->
      match instruction with
      | Move t -> [Move t]

      | Repeat (n, prog_sub) ->
        (* Il faut unfold n fois le programme dans le Repeat *)
        List.concat (List.init n (fun _ -> unfold_repeat prog_sub))

      | Either (prog1, prog2) ->
        (* On unfold les deux sous-programmes du Either *)
        [Either (unfold_repeat prog1, unfold_repeat prog2)]
    ) prog
  )


(* run_det : program -> point -> point list *)
(* Exécute un programme prog déterministe à partir d'un point initial p.
   Retourne la liste des positions visitées. *)
let rec run_det (prog : program) (p : point) : point list =
  let rec execute (prog : program) (current_pos : point) (visited : point list) : point list =
    match prog with
    (* On retourne la liste pour avoir les points visités dans l'ordre d'exécution *)
    | [] -> List.rev visited 
    | instr :: rest -> 
        match instr with
        | Move t ->
          (* On applique la transformation et on continue avec la nouvelle position.*)
          let new_pos = transform t current_pos in
          execute rest new_pos (new_pos :: visited)

        | Repeat (n, sub_prog) ->
          (* `repeat` exécute le sous-programme n fois, et accumule les positions visitées *)
          let rec repeat n acc_pos acc_visited =
            if n = 0 then acc_visited
            else 
              let new_visited = execute sub_prog acc_pos [] in
              let final_pos = List.hd new_visited in
              (* On List.rev_append pour respecter l'ordre d'exécution *)
              repeat (n - 1) final_pos (List.rev_append new_visited acc_visited)
          in
          let final_visited = repeat n current_pos visited in
          (* On utilise List.hd pour récupérer la position courante. *)
          execute rest (List.hd final_visited) final_visited

        | Either (_, _) -> raise NonDeterministicProgram
  in
  execute prog p [p]


(*target_reached_det : program -> point -> rectangle -> bool*)
(* Vérifie si la dernière position atteinte par le robot se trouve dans son trajet cible. *)
let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  (* On renverse la liste pour avoir le dernier élément *)
  match List.rev (run_det prog p) with
  | [] -> false
  | final_pos :: _ -> in_rectangle target final_pos
  

(* run : program -> point -> point list*)
(* Exécute un programme quelconque, même non-déterministe.
   Retourne la liste des positions visitées. Peut être  différente si on l’appelle plusieurs fois ! *)
let run (prog : program) (p : point) : point list =
  let rec execute (prog : program) (current_pos : point) (visited : point list) : point list =
    match prog with
    | [] -> List.rev visited
    | instr :: rest -> 
      match instr with
      | Move t ->
        (* On applique la transformation et on continue avec la nouvelle position. *)
        let new_pos = transform t current_pos in
        execute rest new_pos (new_pos :: visited)

      | Repeat (n, sub_prog) ->
        (* `repeat` exécute le sous-programme n fois, et accumule les positions visitées *)
        let rec repeat n acc_pos acc_visited =
          if n = 0 then acc_visited
          else 
            let new_visited = execute sub_prog acc_pos [] in
            let final_pos = List.hd new_visited in
            (* On List.rev_append pour respecteur l'ordre d'exécution. *)
            repeat (n - 1) final_pos (List.rev_append new_visited acc_visited)
          in
          let final_visited = repeat n current_pos visited in
          (* On utilise List.hd pour récupérer la position courante. *)
          execute rest (List.hd final_visited) final_visited

      (* On choisit aléatoirement entre les deux sous-programmes. *)
      | Either (prog1, prog2) ->
        (* Choix aléatoire *)
        let chosen_prog = if Random.int 2 = 0 then prog1 else prog2 in
        let new_visited = execute chosen_prog current_pos [] in
        let final_pos = List.hd new_visited in
        execute rest final_pos (List.rev_append new_visited visited)
  in
  execute prog p [p]


(*all_choices : program -> program list*)
(* Retourne toutes les combinaisons possibles des choix non-déterministes dans un programme. *)
let rec all_choices (prog : program) : program list =
  match prog with
  | [] -> [[]]
  | instr :: rest ->
    (* Toutes les combinaisons possibles des choix *)
    let rest_choices = all_choices rest in
    match instr with
     
    | Move t -> 
      (* Pas de choix non déterministe pour move.
         Pour chaque choix possible du reste du programme, on ajoute `instr` (Move t) au début de chaque combinaison. *)
      List.map (fun choice -> instr :: choice) rest_choices 

    | Either (prog1, prog2) ->
      (* On récupère toutes les possibilités pour les deux sous-programmes *)
      let choices1 = all_choices prog1 in
      let choices2 = all_choices prog2 in
      (* On concatène les combinaisons de prog1 et prog2 avec les choix du reste du programme. *)
      List.concat [
        List.concat (List.map (fun choice1 -> List.map (fun rest_choice -> choice1 @ rest_choice) rest_choices) choices1);
        List.concat (List.map (fun choice2 -> List.map (fun rest_choice -> choice2 @ rest_choice) rest_choices) choices2);
      ]

    (* Il suffit de déplier le repeat et de rappeler la fonction. *)
    | Repeat _ -> 
      let unfolded_prog = unfold_repeat prog in
      all_choices unfolded_prog
      

(* target_reached : program -> point -> rectangle -> bool*)
(* Vérifie si pour toutes les combinaisons possibles d'exécution d'un programme, 
   le robot termine son trajet dans la zone cible  *)
let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  (* On récupère toutes les combinaison possibles d'exécution *)
  let all_programs = all_choices prog in

  (* On itère sur tous les programmes de la liste*)
  List.for_all (fun program ->
    (* On applique chaque instruction du programme à la position actuelle du robot *)
    let final_position = List.fold_left (fun current_point instruction ->
      match instruction with
      | Move t -> transform t current_point (* On applique la transformation *)
      | Repeat _ -> current_point (* Pas de transformation *)
      | Either (_, _) -> current_point (* Pas de transformation *)
    ) p program in

    (* On vérifie si le robot est dans le rectangle cible *)
    in_rectangle r final_position
  ) all_programs
          