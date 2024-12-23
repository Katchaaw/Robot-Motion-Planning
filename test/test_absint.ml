open Pf5
open Geo
open Interp
open Approx
open Test_base

let pp_vector ppf vec =
  Fmt.pf ppf "{%g,%g}" vec.x vec.y
let vector x y =
  { x; y }

let pp_rectangle ppf r =
  Fmt.pf ppf "{%g,%g,%g,%g}" r.x_min r.x_max r.y_min r.y_max
let rectangle x_min x_max y_min y_max =
  { x_min; x_max; y_min; y_max }

let pp_transformation ppf = function
  | Translate vec ->
      Fmt.pf ppf "TRANS(%a)"
        pp_vector vec
  | Rotate (p, angle) ->
      Fmt.pf ppf "ROT(%a,%g)" 
        pp_vector p
        angle

let rec pp_instruction ppf = function
  | Move op ->
    Fmt.pf ppf "Move %a"
      pp_transformation op
  | Repeat (n, prog) ->
    Fmt.pf ppf "Repeat (%i, %a)"
      n
      pp_program prog
  | Either (prog1, prog2) ->
    Fmt.pf ppf "Either (%a, %a)"
      pp_program prog1
      pp_program prog2
and pp_program ppf =
  Fmt.(brackets @@ list ~sep:(const string "; ") pp_instruction) ppf
    
let float_equal f1 f2 =
  Float.abs (f2 -. f1) < 0.01
let vector_equal vec1 vec2 =
  float_equal vec1.x vec2.x &&
  float_equal vec1.y vec2.y
let rectangle_equal r1 r2 =
  float_equal r1.x_min r2.x_min &&
  float_equal r1.x_max r2.x_max &&
  float_equal r1.y_min r2.y_min &&
  float_equal r1.y_max r2.y_max

let testable_vector =
  Alcotest.testable pp_vector vector_equal
let testable_rectangle =
  Alcotest.testable pp_rectangle rectangle_equal

(* let testable_transformation  = *)
(*   Alcotest.testable pp_transformation (=) *)

let testable_program =
  Alcotest.testable pp_program (=)

let () = add_tests_2
    "translate"
    "translate"
    translate
    pp_vector
    pp_vector
    testable_vector
    [ vector 1. 1., vector 0. 0., vector 1. 1.;
      vector 1. 1., vector 2.5 4.5, vector 3.5 5.5
    ]

let () = add_tests_1
    "deg_of_rad"
    "deg_of_rad"
    deg_of_rad
    Fmt.float
    (Alcotest.float 0.01)
    [ (Float.pi /. 2.), 90.;
      (Float.pi /. 6.), 30.
    ]

let () = add_tests_1
    "rad_of_deg"
    "rad_of_deg"
    rad_of_deg
    Fmt.float
    (Alcotest.float 0.01)
    [ 45., 0.7854;
      180., Float.pi
    ]

let () = add_tests_3
  "rotate"
  "rotate"
  rotate
  pp_vector
  Fmt.float
  pp_vector
  testable_vector
  [ vector 0. 0., 90., vector 1. 1., vector (-1.) 1.;
    vector 0. 1., 45., vector 1. 0., vector 1.414 1.
  ]

let () = add_tests_2
  "transform"
  "transform"
  transform
  pp_transformation
  pp_vector
  testable_vector
  [ Translate (vector 1.5 9.9), vector 0. 0., vector 1.5 9.9;
    Rotate (vector 0. 0., 0.), vector 1. 1., vector 1. 1.;
    Rotate (vector 0. 0., 45.), vector (sqrt 2. /. 2.) (sqrt 2. /. 2.), vector 0. 1.;
    Rotate (vector 2. 1., 180.), vector 0. 0., vector 4. 2.;
  ]

let () = add_tests_2
  "in_rectangle"
  "in_rectangle"
  in_rectangle
  pp_rectangle
  pp_vector
  Alcotest.bool
  [ rectangle 0. 1. 0. 1., vector 0. 0., true;
    rectangle 0. 1. 0. 1., vector 1.2 0.3, false;
    rectangle (-1.) 1. (-1.) 1., vector 0. 0., true;
    rectangle (-1.) 1. (-1.) 1., vector 0. 2., false;
  ]

let () = add_tests_1
  "corners"
  "corners"
  (fun r -> List.sort compare @@ corners r)
  pp_rectangle
  (Alcotest.list testable_vector)
  [ rectangle 0. 1. 0. 1., [vector 0. 0.; vector 0. 1.; vector 1. 0.; vector 1. 1.];
    rectangle (-1.) 1. (-1.) 1., [vector (-1.) (-1.); vector (-1.) 1.; vector 1. (-1.); vector 1. 1.];
  ]

let () = add_tests_1
  "rectangle_of_list"
  "rectangle_of_list"
  rectangle_of_list
  (pp_list pp_vector)
  testable_rectangle
  [ [vector (-1.) 0.; vector 1. 0.], rectangle (-1.) 1. 0. 0.;
    [vector 0. (-1.); vector 0. 1.], rectangle 0. 0. (-1.) 1.;
    [vector (-5.) 42.; vector 10. (-12.)], rectangle (-5.) 10. (-12.) 42.;
    [vector 0. 1.; vector 1. 0.; vector 2. (-1.); vector (-1.) 2.5; vector (-3.) 0.; vector (-1.5) 2.; vector 2. 4.], rectangle (-3.) 2. (-1.) 4.;
  ]

let () = add_tests_1
  "is_deterministic"
  "is_deterministic"
  is_deterministic
  pp_program
  Alcotest.bool
  [ [Repeat (8, [Move (Rotate (vector 0. 0., 45.))]); Move (Translate (vector 1. 1.))], true;
    [Move (Translate (vector 1. 1.)); Either ([Move (Rotate (vector 0. 0., 90.))], [Move (Rotate (vector 1. 1., -90.))])], false;
  ]

let () =
  let tr1 = Move (Translate (vector 1. 1.)) in
  let tr2 = Move (Translate (vector 2. 2.)) in
  add_tests_1
  "unfold_repeat"
  "unfold_repeat"
  unfold_repeat
  pp_program
  testable_program
  [ [], [];
    [tr1], [tr1];
    [tr1; Repeat (0, [tr2]); tr1], [tr1; tr1];
    [tr1; Repeat (3, [tr2]); tr1], [tr1; tr2; tr2; tr2; tr1];
  ]

let () = add_tests_2
  "run_det"
  "run_det"
  run_det
  pp_program
  pp_vector
  (Alcotest.list testable_vector)
  [ [], vector 0. 0., [vector 0. 0.];
    [Move (Translate (vector 1. 0.)); Move (Translate (vector 0. 1.))], vector 1. 2., [vector 1. 2.; vector 2. 2.; vector 2. 3.];
    [Move (Rotate (vector 0. 0., 90.)); Move (Rotate (vector 1. 1., 90.))], vector 1. 1., [vector 1. 1.; vector (-1.) 1.; vector 1. (-1.)];
    [Repeat (5, [Move (Translate (vector 1. 0.))])], vector 0. 0., [vector 0. 0.; vector 1. 0.; vector 2. 0.; vector 3. 0.; vector 4. 0.; vector 5. 0.];
  ]

let () = add_tests_3
  "target_reached_det"
  "target_reached_det"
  target_reached_det
  pp_program
  pp_vector
  pp_rectangle
  Alcotest.bool
  [ [], vector 0. 0., rectangle (-1.) 1. (-1.) 1., true;
    [Move (Translate (vector 1. 0.)); Move (Translate (vector 0. 1.))], vector 1. 2., rectangle 1.5 2.5 2.5 3.5, true;
    [Move (Rotate (vector 0. 0., 90.)); Move (Rotate (vector 1. 1., 90.))], vector 1. 1., rectangle 0.5 1.5 (-1.5) 1.5, true;
    [Repeat (100, [Move (Translate (vector 1. 1.))])], vector 0. 0., rectangle 99.9 100.1 99.9 100.1, true;
  ]

let () = add_tests_2
  "run"
  "run"
  run
  pp_program
  pp_vector
  (Alcotest.list testable_vector)
  [ [], vector 0. 0., [vector 0. 0.];
    [Repeat (4, [Move (Rotate (vector 0. 0., 90.))])], vector 1. 0., [vector 1. 0.; vector 0. 1.; vector (-1.) 0.; vector 0. (-1.); vector 1. 0.];
  ]

let () =
  let t1 = Move (Translate (vector 0. 1.)) in
  let t2 = Move (Translate (vector 1. 0.)) in
  let rot = Move (Rotate (vector 0. 0., 45.)) in
  add_tests_1
  "all_choices"
  "all_choices"
  (fun prog -> List.sort_uniq compare @@ all_choices prog)
  pp_program
  Alcotest.(list @@ testable_program)
  [ [], [[]];
    [t1], [[t1]];
    [rot; Either ([t1], [t2])], [[rot; t1]; [rot; t2]];
    [Repeat (3, [Either ([t1], [t2])])], [[t1;t1;t1]; [t1;t1;t2]; [t1;t2;t1]; [t1;t2;t2]; [t2;t1;t1]; [t2;t1;t2]; [t2;t2;t1]; [t2;t2;t2]];
  ]

let () =
  let t1 = Move (Translate (vector 0. 1.)) in
  let t2 = Move (Translate (vector 1. 0.)) in
  let t3 = Move (Translate (vector 0. (-1.))) in
  let t4 = Move (Translate (vector (-1.) 0.)) in
  add_tests_3
  "target_reached"
  "target_reached"
  target_reached
  pp_program
  pp_vector
  pp_rectangle
  Alcotest.bool
  [ [Repeat (10, [Either ([t1; t2], [t2; t1])])], vector 0. 0., rectangle 9.9 10.1 9.9 10.1, true;
    [Repeat (5, [Either ([t1],[t3]); Either ([t2],[t4])])], vector 0. 0., rectangle (-4.9) 4.9 (-4.9) 4.9, false;
  ]

let () =
  let up_to_one_half f1 f2 = Float.abs (f2 -. f1) < 0.5 in
  let vector_cmp vec1 vec2 =
    up_to_one_half vec1.x vec2.x &&
    up_to_one_half vec1.y vec2.y
  in
  add_tests_1
  "sample"
  "sample"
  sample
  pp_rectangle
  (Alcotest.testable pp_vector vector_cmp)
  [ rectangle 0. 1. 0. 1., vector 0.5 0.5;
    rectangle 4.5 5.5 4.5 5.5, vector 5. 5.;
  ]

let () = add_tests_2
  "transform_rect"
  "transform_rect"
  transform_rect
  pp_transformation
  pp_rectangle
  testable_rectangle
  [ Translate (vector 1.5 9.9), rectangle 0. 1. 0. 1., rectangle 1.5 2.5 9.9 10.9;
    Translate (vector (-1.) (-1.)), rectangle 0. 2. 0. 2., rectangle (-1.) 1. (-1.) 1.;
    Rotate (vector 0. 0., 45.), rectangle 0. 1. 0. 1., rectangle (-0.707) 0.707 0. 1.414;
    Rotate (vector 0. 0., 90.), rectangle 0. 1. 0. 1., rectangle (-1.) 0. 0. 1.;
  ]

let () = add_tests_2
  "run_rect"
  "run_rect"
  run_rect
  pp_program
  pp_rectangle
  (Alcotest.list testable_rectangle)
  [ [Move (Translate (vector 2. 1.))], rectangle 0. 1. 0. 1., [rectangle 0. 1. 0. 1.; rectangle 2. 3. 1. 2.];
    [Repeat (4, [Move (Rotate (vector 0. 0., 45.))])], rectangle (-1.) 1. (-1.) 1., [rectangle (-1.) 1. (-1.) 1.; rectangle (-1.414) 1.414 (-1.414) 1.414; rectangle (-2.) 2. (-2.) 2.; rectangle (-2.828) 2.828 (-2.828) 2.828; rectangle (-4.) 4. (-4.) 4.];
  ]

let () = add_tests_2
  "inclusion"
  "inclusion"
  inclusion
  pp_rectangle
  pp_rectangle
  Alcotest.bool
  [ rectangle 1. 2. 1. 2., rectangle 0. 3. 0. 3., true;
    rectangle 0. 1. 0. 1., rectangle 0.5 1.5 0.5 1.5, false;
    rectangle 0. 1. 0. 1., rectangle 0. 1. 0. 1., true;
    rectangle 0. 3. 0. 3., rectangle 1. 2. 1. 2., false;
  ]

let () =
  let t1 = Move (Translate (vector 0. 1.)) in
  let t2 = Move (Translate (vector 1. 0.)) in
  add_tests_3
  "target_reached_rect"
  "target_reached_rect"
  target_reached_rect
  pp_program
  pp_rectangle
  pp_rectangle
  Alcotest.bool
  [ [Repeat (10, [Either ([t1; t2], [t2; t1])])], rectangle 0. 0. 0. 0., rectangle 9.9 10.1 9.9 10.1, true;
    [Repeat (20, [Move (Rotate (vector 0. 0., 45.))])], rectangle (-1.) 1. (-1.) 1., rectangle (-1000.) 1000. (-1000.) 1000., false;
  ]

let () = add_tests_2
  "run_polymorphe transform"
  "run_polymorphe transform"
  (run_polymorphe transform)
  pp_program
  pp_vector
  (Alcotest.list testable_vector)
  [ [], vector 0. 0., [vector 0. 0.];
    [Repeat (4, [Move (Rotate (vector 0. 0., 90.))])], vector 1. 0., [vector 1. 0.; vector 0. 1.; vector (-1.) 0.; vector 0. (-1.); vector 1. 0.];
  ]

let () = add_tests_2
  "run_polymorphe transform_rect"
  "run_polymorphe transform_rect"
  (run_polymorphe transform_rect)
  pp_program
  pp_rectangle
  (Alcotest.list testable_rectangle)
  [ [Move (Translate (vector 2. 1.))], rectangle 0. 1. 0. 1., [rectangle 0. 1. 0. 1.; rectangle 2. 3. 1. 2.];
    [Repeat (4, [Move (Rotate (vector 0. 0., 45.))])], rectangle (-1.) 1. (-1.) 1., [rectangle (-1.) 1. (-1.) 1.; rectangle (-1.414) 1.414 (-1.414) 1.414; rectangle (-2.) 2. (-2.) 2.; rectangle (-2.828) 2.828 (-2.828) 2.828; rectangle (-4.) 4. (-4.) 4.];
  ]

let () =
  add_tests_2
  "over_approximate"
  "over_approximate"
  over_approximate
  pp_program
  pp_rectangle
  testable_rectangle
  [ [], rectangle 1. 2. 3. 4., rectangle 1. 2. 3. 4.;
    [Either ([Move (Translate (vector 1. 2.))], [Move (Rotate (vector 0. 0., 90.))])], rectangle 0. 1. 0. 1., rectangle (-1.) 2. 0. 3.;
    [Repeat (100, [Either ([Move (Translate (vector 1. 0.))], [Move (Translate (vector 0. 1.))])])], rectangle 0. 0. 0. 0., rectangle 0. 100. 0. 100.;
  ]
  
let () =
  let t1 = Move (Translate (vector 0. 1.)) in
  let t2 = Move (Translate (vector 1. 0.)) in
  let t3 = Move (Translate (vector 0. (-1.))) in
  let t4 = Move (Translate (vector (-1.) 0.)) in
  add_tests_3
  "feasible_target_reached"
  "feasible_target_reached"
  feasible_target_reached
  pp_program
  pp_rectangle
  pp_rectangle
  Alcotest.bool
  [ [], rectangle 0. 0. 0. 0., rectangle (-1.) 1. (-1.) 1., true;
    [Move (Translate (vector 2. 2.)); Move (Rotate (vector 0. 0., 60.)); Move (Translate (vector (-1.) 4.))], rectangle 0. 0.3 0. 0.3, rectangle (-2.) (-1.5) 6.5 7.5, true;
    [Either ([Move (Translate (vector 5. 1.))], [Move (Translate (vector 1. 5.))])], rectangle (-0.5) 0.5 (-0.5) 0.5, rectangle 0. 2. 4. 6., false;
    [Repeat (100, [Either ([t1; t2], [t2; t1])])], rectangle 0. 0. 0. 0., rectangle 99.9 100.1 99.9 100.1, true;
    [Repeat (100, [Either ([t1],[t3]); Either ([t2],[t4])])], rectangle 0. 0. 0. 0., rectangle (-99.9) 99.9 (-99.9) 99.9, false;
  ]
