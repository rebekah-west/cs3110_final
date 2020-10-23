(** AF:
    RI:
*)
type hole_number = int
type hole_location = int * int


(** AF:
    RI:
    will be implmented later as an extra feature
*)
type terrain_type =
  | Lake
  | Tree
  | Sand

type terrain = {
  name: string;
  location: int * int;
  size: string;
}

(** AF:
    RI:
*)
type hole = {
  hole_number: hole_number;
  par_number: int;
  hole_location: hole_location;
  description: string;
  terrain: terrain list
}


(** AF:
    RI:
*)
type t = {
  holes: hole array;
  difficulty: string;
}

(** The type of wind representing the strength and direction *)
type wind = int * float

exception UnknownHole of hole_number

let tuple_of_string string = 
  let lst = String.split_on_char ',' string in
  (int_of_string (List.hd lst), int_of_string (List.nth lst 1))

let terrain_of_json j =
  let open Yojson.Basic.Util in {
    name = j |> member "name" |> to_string;
    location = j |> member "location" |> to_string |> tuple_of_string;
    size = j |> member "size" |> to_string;
  }

let holes_of_json j =
  let open Yojson.Basic.Util in {
    hole_number = j |> member "hole_number" |> to_int;
    par_number = j |> member "par_number" |> to_int;
    hole_location = j |> member "hole_location" |> to_string |> tuple_of_string;
    description = j |> member "description" |> to_string;
    terrain = j |> member "terrain" |> to_list |> List.map terrain_of_json;
  }


let from_json j =
  let open Yojson.Basic.Util in {
    holes = j |> member "holes" |> to_list |> Array.of_list |> Array.map holes_of_json;
    difficulty = j |> member "difficulty" |> to_string;
  }

let start_hole course = (course.holes).(0).hole_number

let num_holes course = Array.length course.holes

let get_hole course hole_number = 
  let holes = course.holes in 
  holes.(hole_number)

let get_hole_loc course hole_number =
  let hole = get_hole course hole_number in 
  hole.hole_location

let difficulty course = course.difficulty

let description course hole_number =
  let hole = get_hole course hole_number in 
  hole.description

let wind () = 
  failwith "unimplemented"