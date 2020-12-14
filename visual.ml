(*********************************************************************)
(*  The hole is 500 x 500. 
    The visual is 52 wide (2 taken off for the lines on the sides). 
    Each width step represents 10 units. 
    The visual is 15 rows tall. Each height step represents 33 units  *)
(*********************************************************************)

let top = " __________________________________________________"
let bottom = "|__________________________________________________|\n\n"
let normal_line = "|                                                  |"
let player = "p"
let hole = "h"
let edge = "|"
let width = 51
let global_rows = 14

let get_row x_cord = let x = Float.to_int (x_cord /. 33.33) in 
  if x = (global_rows+1) then global_rows else x
let get_col y_cord = let y = Float.to_int (y_cord /. 10.) in 
  if y=(width-1) then (width-2) else y
let get_row_col char = function
  | (x,y) -> (get_row x, get_col y, char)
let get_coords (x,y,c) = (get_row x, get_col y, c)

(* [loc_sort loc loc] is a comparison function to be used in List.sort *)
let loc_sort (x1, y1, c1) (x2, y2, c2) = 
  if x1<x2 then ~-1
  else begin 
    if x1>x2 then 1
    else 0 end
let y_sort (x1, y1, c1) (x2, y2, c2) = 
  if y1<y2 then ~-1
  else begin 
    if y1>y2 then 1
    else 0 end

let append start stop ref character =
  for i=(start+1) to stop do 
    ref := !ref ^ character
  done

(* adds empty lines to [string] until the bottom of the hole 
   [num] lines have already been completed *)
let finish num str = 
  append num global_rows str ("\n" ^ normal_line);
  str := !str  ^ "\n" ^ bottom;
  !str

let finish_row row = 
  let start = String.length !row in 
  append start width row " ";
  row := !row ^ edge;
  !row

let add_normal str = 
  str := !str ^ "\n" ^ normal_line;
  str

let rec up_to_char list acc row = match list with
  | [] -> failwith "impossible" 
  | (x1,y1,c1)::(x2,y2,c2)::t -> begin
      append acc y1 row " ";
      row := !row ^ c1;
      let counter = String.length !row in
      up_to_char ((x2,y2,c2)::t) counter row
    end
  | (x1,y1,c1)::t -> begin
      append acc y1 row " ";
      row := !row ^ c1;
      row := finish_row row;
      !row
    end

let row_with_multiple list str = 
  let row = ref edge in 
  row := up_to_char list 0 row;
  str := !str ^ "\n" ^ !row;
  !str

let row_equal a (x,y,s) = if a=x then true else false 
let list_has_row_equal a lst = List.exists (row_equal a) lst

let add_row locs i str = 
  let rowmates = List.find_all (row_equal i) locs in begin
    match rowmates with 
    | [] -> failwith "impossible"
    | (x1,y1,c1)::t -> begin
        let sorted_mates = (List.sort y_sort rowmates) in
        str := (row_with_multiple sorted_mates str)
      end
  end;
  !str

(* [iter_locs l a s] iterates through the rows of the grid and prints a line 
   with a marker each time the first element in locations has the same 
   row number as the current row of the grid. *)
let iter_locs locs num_completed_rows str =
  for i = 0 to global_rows do 
    if list_has_row_equal i locs 
    then str := add_row locs i str
    else str := !(add_normal str)
  done;
  str := !str  ^ "\n" ^ bottom;
  !str

let print_loc hole player obst = 
  let hole_loc = get_row_col "h" hole in 
  let player_loc = get_row_col "p" player in
  let obstacle_locs = List.map get_coords obst in 
  let raw_locs =  hole_loc::player_loc::obstacle_locs in
  let sorted_locs = (List.sort loc_sort raw_locs) in 
  let str = ref top in 
  str := (iter_locs sorted_locs 1 str);
  print_string !str