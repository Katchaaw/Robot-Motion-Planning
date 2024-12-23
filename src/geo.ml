(* Code de la Section 3 du projet. *)

(*############## Définition des types ##############*)

type coord2D = {
    x : float;
    y : float
}
type point = coord2D
type vector = coord2D
type angle = float

type transformation =
  Translate of vector
| Rotate of point * angle

type rectangle = {
    x_min : float;
    x_max : float;
    y_min : float;
    y_max : float
}


(*############ Début du code ##############*)

(* translate : vector -> point -> point *)
(* Renvoie le point résultant de la translation entre un vecteur et un point. *)
let translate (v: vector) (p: point) : point = 
  { x = p.x +. v.x; y = p.y +. v.y }


(* rad_of_deg : angle -> angle *)
(* Convertit un angle de degrés en radians. *)
let rad_of_deg (deg: angle) : angle = 
  deg *. Float.pi /. 180.0
  

(* deg_of_rad : angle -> angle *)
(* Convertit un angle de radians en degrés. *)
let deg_of_rad (rad: angle) : angle = 
  rad *. 180.0 /. Float.pi


(* rotate : point -> angle -> point -> point *)
(* Renvoie le point résultant de la rotation d'un point p autour
   d'un centre c d'un angle alpha (en degrés) donné en parmètres. *)
let rotate (c: point) (alpha: angle) (p: point) : point =
  let theta = rad_of_deg alpha in
  let cos_theta = cos theta in
  let sin_theta = sin theta in
  (*Formules de rotation matricielle*)
  { x = c.x +. (p.x -. c.x) *. cos_theta -. (p.y -. c.y) *. sin_theta;
    y = c.y +. (p.x -. c.x) *. sin_theta +. (p.y -. c.y) *. cos_theta }


(* transform : transformation -> point -> point *)
(* Applique une transformation (translation ou rotation) sur un point. *)
let transform (t: transformation) (p: point) : point =
  match t with
  | Translate v -> translate v p
  | Rotate (c, alpha) -> rotate c alpha p


(* in_rectangle : rectangle -> point -> bool *)
(* Détermine si un point est à l'intérieur, ou sur les bords d'un rectangle. *)
let in_rectangle (r : rectangle) (p : point) : bool =
  p.x >= r.x_min && p.x <= r.x_max &&
  p.y >= r.y_min && p.y <= r.y_max


(* corners : rectangle -> point list *)
(* Renvoie la liste de ses quatre sommets d'un rectangle. *)
let corners (r :rectangle) : point list =
  let down_left = {x = r.x_min ; y = r.y_min} in
  let down_right = {x = r.x_max ; y = r.y_min} in
  let up_left = {x = r.x_min ; y = r.y_max} in
  let up_right = {x = r.x_max ; y = r.y_max} in
  [down_left; down_right ; up_left; up_right]
  

(* rectangle_of_list : point list -> rectangle *)
(* Renvoie le plus petit rectangle contenant tous les points de la liste. *)
let rectangle_of_list (pl : point list) : rectangle = 
  match pl with
  |[] -> failwith "La liste de points est vide"

  |p :: ps ->
    (* Utilisation de fold_left pour trouver les min et max des coordonnées x et y *)
    (* À chaque point de la liste, on met à jour les bornes min et max de telle sorte que :
       - Pour x_min : On garde le minimum entre le minimum actuel et la coordonée 
                      x du point que l'on regarde (respectivement y_min et y)
        -Pour x_max : On garde le maximum entre le maximum actuel et la coordonée 
                      x du point que l'on regarde (respectivement y_max et y) *)
    let (x_min, x_max, y_min, y_max) =
      List.fold_left(fun (xmin, xmax, ymin, ymax) point ->
        (min xmin point.x, max xmax point.x, min ymin point.y, max ymax point.y)
      ) (p.x, p.x, p.y, p.y) ps 
      
  in
  { x_min; x_max; y_min; y_max }
