(*****************************************************)
(* Implementations of functions related to a course *)
(*****************************************************)

(* AF: The variant type terrain_type represents the 3 different types of 
    potential terrain in a golf course: lake, tree, and sand. *)
type terrain_type =
  | Lake
  | Tree
  | Sand

type terrain = {
  name: string;
  location: int * int;
  size: string;
}

type hole_number = int
type hole_location = float * float

type hole = {
  hole_number: hole_number;
  par_number: int;
  hole_location: hole_location;
  description: string;
  terrain: terrain list
}

type t = {
  holes: hole array;
  difficulty: string;
}

(** The type of wind representing the strength and direction *)
type wind = int * float

exception UnknownHole of hole_number

(* [tuple_of_string string] creates an integer tuple from string [string] *)
let tuple_of_string string = 
  let lst = String.split_on_char ',' string in
  match lst with 
  | [] -> failwith "empty string"
  | [h] -> failwith "only one element in the string"
  | h::[t] -> (int_of_string h, int_of_string t)
  | _ -> failwith "too many elements"

(* [float_of_int_tuple tup] converts an integer tuple to a float tuple. *)
let float_of_int_tuple tup = 
  let float_tup = (tup |> fst |> float_of_int, tup |> snd |> float_of_int) in 
  float_tup 


(* [terrain_of_json j] creates a terrain object from json [j] *)
let terrain_of_json j =
  let open Yojson.Basic.Util in {
    name = j |> member "name" |> to_string;
    location = j |> member "location" |> to_string |> tuple_of_string;
    size = j |> member "size" |> to_string;
  }

(* [hole_of_json j] creates a hole object from json [j] *)
let hole_of_json j =
  let open Yojson.Basic.Util in {
    hole_number = j |> member "hole_number" |> to_int;
    par_number = j |> member "par_number" |> to_int;
    hole_location = j |> member "hole_location" |> to_string |> tuple_of_string
                    |> float_of_int_tuple;
    description = j |> member "description" |> to_string;
    terrain = j |> member "terrain" |> to_list |> List.map terrain_of_json;
  }


let from_json j =
  let open Yojson.Basic.Util in {
    holes = j |> member "holes" |> to_list |> Array.of_list 
            |> Array.map hole_of_json;
    difficulty = j |> member "difficulty" |> to_string;
  }

let start_hole course = (course.holes).(0).hole_number

let num_holes course = Array.length course.holes

let get_holes course = course.holes

let get_hole course hole_number = 
  if not (hole_number <= Array.length course.holes) 
  then raise (UnknownHole (hole_number))
  else course.holes.(hole_number-1)

let get_hole_loc course hole_number =
  let hole = get_hole course hole_number in 
  hole.hole_location

let get_par course hole_number =  
  let hole = get_hole course hole_number in 
  hole.par_number

let get_hole_par hole = hole.par_number

let difficulty course = course.difficulty

let description course hole_number =
  let hole = get_hole course hole_number in 
  hole.description

let wind () = 
  failwith "unimplemented"

let get_hole_number hole = hole.hole_number