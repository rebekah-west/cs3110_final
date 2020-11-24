(*********************************************************************)
(** The hole is 500 x 500. 
    The visual is 52 wide (2 taken off for the lines on the sides). 
    Each width step represents 10 units. 
    The visual is 15 rows tall. Each height step represents 33 units  *)
(*********************************************************************)
let pp_loc (x,y,c) = 
  let s = "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", " ^ c ^ ")" in 
  print_string s

let top = " _________________________________________________"
let bottom = "|_________________________________________________|\n\n"
let normal_line = "|                                                 |"
let player = "p"
let hole = "h"
let edge = "|"

let get_row x_cord = Float.to_int (x_cord /. 33.33)
let get_col y_cord = Float.to_int (y_cord /. 10.)
let get_row_col char = function
  | (x,y) -> (get_row x, get_col y, char)

let get_min_row str (x1,y1,c1) (x2,y2,c2) = 
  match str with 
  | "min" -> if x1 <= x2 then (x1,y1,c1) else (x2,y2,c2)
  | "max" -> if x1 > x2 then (x1,y1,c1) else (x2,y2,c2)
  | _ -> raise (Invalid_argument str)

let get (x,y,c) find = 
  match find with
  | "row" -> x
  | "col" -> y
  | _ -> raise (Invalid_argument find)

let construct_row (x,y,c) = 
  let string = ref edge in 
  for i=1 to y do 
    string := !string ^ " "
  done;
  string := !string ^ c;
  for i=y to 47 do 
    string := !string ^ " "
  done;
  string := !string ^ edge;
  !string

let get_char (x, y, c) = c

let print_loc hole player = 
  let hole_loc = get_row_col "h" hole in 
  let player_loc = get_row_col "p" player in
  let min = get_min_row "min" hole_loc player_loc in
  let max = get_min_row "max" hole_loc player_loc in
  let string = ref top in 
  for i=1 to (get min "row" -1) do 
    string := !string ^ "\n" ^ normal_line
  done;
  string := !string ^ "\n" ^ (construct_row min);
  for i=(get min "row" +1) to (get max "row" -1) do 
    string := !string ^ "\n" ^ normal_line
  done;
  string := !string ^ "\n" ^ (construct_row max);
  for i=(get max "row") to 14 do 
    string := !string ^ "\n" ^ normal_line
  done;
  string := !string ^ "\n" ^ bottom;
  print_string !string