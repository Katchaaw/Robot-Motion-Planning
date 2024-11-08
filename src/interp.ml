open Geo

(* Code de la Section 4 du projet. *)
exception NonDeterministicProgram

type instruction =
  Move of transformation
| Repeat of int * program
| Either of program * program 
and program = instruction list

(* Module Random pour les choix aléatoires *)
let () = Random.self_init ()


let rec is_deterministic (prog : program) : bool =
  List.for_all (fun instruction ->
    match instruction with
    | Move _ -> true
    | Repeat (_, prog_sub) -> is_deterministic prog_sub
    | Either (_, _) -> false
  ) prog

let rec unfold_repeat (prog : program) : program =
  List.flatten (
    List.map (fun instruction ->
      match instruction with
      | Move t -> [Move t]
      | Repeat (n, prog_sub) ->
          (* Dépliage de Repeat : on répète prog_sub déplié n fois *)
          List.concat (List.init n (fun _ -> unfold_repeat prog_sub))
      | Either (prog1, prog2) ->
          (* Dépliage des Repeat dans chaque branche de Either *)
          [Either (unfold_repeat prog1, unfold_repeat prog2)]
    ) prog
  )


let rec run_det (prog : program) (p : point) : point list =
  let rec execute (prog : program) (current_pos : point) (visited : point list) : point list =
    match prog with
    | [] -> List.rev visited  (* On retourne la liste complète des positions visitées *)
    | instr :: rest -> 
        match instr with
        | Move t ->
            let new_pos = transform t current_pos in
            execute rest new_pos (new_pos :: visited)
        | Repeat (n, sub_prog) ->
            let rec repeat n acc_pos acc_visited =
              if n = 0 then acc_visited
              else 
                let new_visited = execute sub_prog acc_pos [] in
                let final_pos = List.hd new_visited in
                repeat (n - 1) final_pos (List.rev_append new_visited acc_visited)
            in
            let final_visited = repeat n current_pos visited in
            execute rest (List.hd final_visited) final_visited
        | Either (_, _) -> raise NonDeterministicProgram
  in
  execute prog p [p]


let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  match List.rev (run_det prog p) with
  | [] -> false  (* Cas improbable : run_det devrait toujours retourner au moins une position *)
  | final_pos :: _ -> in_rectangle target final_pos
  
  
let rec run (prog : program) (p : point) : point list =
  let rec execute (prog : program) (current_pos : point) (visited : point list) : point list =
    match prog with
    | [] -> List.rev visited  (* Retourne la liste des positions visitées *)
    | instr :: rest -> 
      match instr with
      | Move t ->
        let new_pos = transform t current_pos in
        execute rest new_pos (new_pos :: visited)
      | Repeat (n, sub_prog) ->
        let rec repeat n acc_pos acc_visited =
          if n = 0 then acc_visited
          else 
            let new_visited = execute sub_prog acc_pos [] in
            let final_pos = List.hd new_visited in
            repeat (n - 1) final_pos (List.rev_append new_visited acc_visited)
          in
          let final_visited = repeat n current_pos visited in
          execute rest (List.hd final_visited) final_visited
      | Either (prog1, prog2) ->
        let chosen_prog = if Random.bool () then prog1 else prog2 in
        let new_visited = execute chosen_prog current_pos [] in
        let final_pos = List.hd new_visited in
        execute rest final_pos (List.rev_append new_visited visited)
  in
  execute prog p [p]

let rec all_choices (prog : program) : program list =
  match prog with
  | [] -> [[]]
  | instr :: rest ->
    let rest_choices = all_choices rest in
    match instr with
    | Move t -> List.map (fun choice -> instr :: choice) rest_choices
    | Either (prog1, prog2) ->
      let choices1 = all_choices prog1 in
      let choices2 = all_choices prog2 in
      List.concat [
        List.concat (List.map (fun choice1 -> List.map (fun rest_choice -> choice1 @ rest_choice) rest_choices) choices1);
        List.concat (List.map (fun choice2 -> List.map (fun rest_choice -> choice2 @ rest_choice) rest_choices) choices2);
      ]
    | Repeat _ -> 
          let unfolded_prog = unfold_repeat prog in
          all_choices unfolded_prog
      
let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  let all_programs = all_choices prog in
  List.for_all (fun program ->
    let final_position = List.fold_left (fun current_point instruction ->
      match instruction with
      | Move t -> transform t current_point
      | Repeat _ -> current_point
      | Either (_, _) -> current_point 
    ) p program in
  in_rectangle r final_position
  ) all_programs
          