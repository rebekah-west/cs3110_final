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
let width = 49

let get_char (x, y, c) = c
let get_row x_cord = Float.to_int (x_cord /. 33.33)
let get_col y_cord = let y = Float.to_int (y_cord /. 10.) in 
  if y=50 then 49 else y
let get_row_col char = function
  | (x,y) -> (get_row y, get_col x, char)
let get_coords (x,y,c) = (get_row y, get_col x, c)
let same_row (x1, y1, c1) (x2, y2, c2) = if x1=x2 then true else false

(* [loc_sort loc loc] is a comparison function to be used in List.sort *)
let loc_sort (x1, y1, c1) (x2, y2, c2) = 
  if x1<x2 then ~-1
  else if x1>x2 then 1
  else 0
let y_sort (x1, y1, c1) (x2, y2, c2) = 
  if y1<y2 then ~-1
  else if y1>y2 then 1
  else 0

let get (x,y,c) find = 
  match find with
  | "row" -> x
  | "col" -> y
  | _ -> raise (Invalid_argument find)

let append start stop ref character =
  for i=(start+1) to stop do 
    ref := !ref ^ character
  done

(* adds empty lines to [string] until the bottom of the hole 
   [num] lines have already been completed *)
let finish num string = 
  append num 14 string ("\n" ^ normal_line);
  string := !string  ^ "\n" ^ bottom;
  !string

let finish_row num row = 
  append num width row " ";
  row := !row ^ edge;
  !row

(* [construct_row (x,y,c)] constructs a row with the character c at y 
   coordinate y and adds it to the string stored in ref [string].
   Returns the new string stored in ref [string]  *)
let construct_row string (x,y,c) = 
  let row = ref edge in 
  append 0 y row " ";
  row := !row ^ c;
  row := finish_row y row;
  string := !string ^ "\n" ^ !row;
  !string

let rec find_rowmates (x1,y1,c1) list acc = match list with 
  | [] -> acc, list
  | (x2,y2,c2)::t -> begin 
      if x1=x2 
      then find_rowmates (x2,y2,c2) t ((x2,y2,c2)::acc)
      else acc, list
    end

let rec up_to_char list acc row = match list with
  | [] -> failwith "impossible" 
  | (x1,y1,c1)::(x2,y2,c2)::t -> begin
      append acc y1 row " ";
      row := !row ^ c1;
      up_to_char ((x2,y2,c2)::t) (y1+1) row
    end
  | (x1,y1,c1)::t -> begin
      append acc y1 row " ";
      row := !row ^ c1;
      row := finish_row y1 row;
      !row
    end


let row_with_multiple list string = 
  let row = ref edge in 
  row := up_to_char list 0 row;
  string := !string ^ "\n" ^ !row;
  !string

let check_next (x1,y1,c1) list = match list with 
  | [] -> false
  | (x2,y2,c2)::t -> if x1=x2 then true else false

let add_normal string = 
  string := !string ^ "\n" ^ normal_line;
  string

(* [iter_locs l a s] iterates through the rows of the grid and prints a line 
   with a marker each time the first element in locations has the same 
   row number as the current row of the grid. *)
let rec iter_locs locs num_completed_rows string = match locs with 
  | [] -> finish num_completed_rows string
  | (x1,y1,c1)::t -> begin
      if x1=num_completed_rows 
      then add_row (x1,y1,c1) t num_completed_rows string
      else iter_locs locs (num_completed_rows+1) (add_normal string)
    end

and add_row elem t num_comp string = 
  let rowmates, rest = find_rowmates elem t [] in begin
    match rowmates with 
    | [] -> string := construct_row string elem
    | (x1,y1,c1)::t -> begin
        let sorted_mates = (List.sort y_sort (elem::rowmates)) in
        string := (row_with_multiple (sorted_mates) string)
      end
  end;
  iter_locs rest (num_comp + 1) string


let print_loc hole player obstacles = 
  let hole_loc = get_row_col "h" hole in 
  let player_loc = get_row_col "p" player in
  let obstacle_locs = List.map get_coords obstacles in 
  let raw_locs =  hole_loc::player_loc::obstacle_locs in
  let sorted_locs = (List.sort loc_sort raw_locs) in 
  let string = ref top in 
  string := (iter_locs sorted_locs 1 string);
  print_string !string



let get_player_loc player = 
  let loc = Player.get_player_location player in 
  let str = Char.escaped (String.get (Player.get_player_name player) 0) in
  get_row_col str loc

let print_all hole player_list obstacles = 
  let hole_loc = get_row_col "h" hole in 
  let player_locs = List.map get_player_loc player_list in
  let obstacle_locs = List.map get_coords obstacles in  
  let raw_locs =  hole_loc::player_locs@obstacle_locs in
  let sorted_locs = (List.sort loc_sort raw_locs) in 
  let string = ref top in 
  string := (iter_locs sorted_locs 1 string);
  print_string !string

(** Examples:
    print_loc (400.,300.) (450.,300.) ([(200.,300.,"w"); (500.,150., "t"); (450., 250., "r"); (500., 300., "e")]);;
    print_loc (300.,300.) (400.,300.);;*)
