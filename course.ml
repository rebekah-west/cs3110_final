(** AF:
    RI:
*)
type hole_number = int


(** AF:
    RI:
*)
type terrain =
  | Lake
  | Tree
  | Sand

(** AF:
    RI:
*)
type hole = {
  hole_number: hole_number;
  par_number: int;
  hole_location: int * int;
  description: string;
  terrain: terrain list
}

type difficulty = {
  (*A placeholder*)
  diff : int;
}
(** AF:
    RI:
*)
type t = {
  holes: hole array;
  difficulty: difficulty;
}

(** The type of wind representing the strength and direction *)
type wind = int * float


exception UnknownHole of hole_number


let from_json j =
  failwith "unimplemented"

let start_hole course = 
  failwith "unimplemented"

let num_holes course =
  failwith "unimplemented"

let get_hole course hole_number = 
  let holes = course.holes in 
  holes.(hole_number)

let get_hole_location course hole_number =
  let hole = get_hole course hole_number in 
  hole.hole_location

let difficulty course = course.difficulty

let description course hole_number =
  let hole = get_hole course hole_number in 
  hole.description

let wind () = 
  failwith "unimplemented"