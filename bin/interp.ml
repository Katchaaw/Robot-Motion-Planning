open Graphics;;

open_graph " 500x500";;

exception Quit;;

let rec loop t =
  let eve = wait_next_event [Mouse_motion;Key_pressed]
  in
  if eve.keypressed
  then 
    match eve.key with
      | 'b'  -> set_color black; loop t
      | 'r'  -> set_color red; loop t
      | 'g'  -> set_color green; loop t
      | 'q'  -> raise Quit
      | '0'..'9' as  x -> loop (int_of_string (String.make 1 x))
      | _    -> loop t
  else begin
    fill_circle (eve.mouse_x-t/2) (eve.mouse_y-t/2) t;
    loop t
  end

let () =
  try loop 5
  with Quit -> close_graph ();;
