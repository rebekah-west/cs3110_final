(** AF:
    RI:
 *)
type t = {
  holes: hole list;
  difficulty: difficulty
}

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


exception UnknownHole of hole_number


let from_json j =
  failwith "unimplemented"

let start_hole = 
  failwith "unimplemented"

let num_holes =
  failwith "unimplemented"

let difficulty =
  failwith "unimplemented"

let description =
  failwith "unimplemented"

let wind = 
  failwith "unimplemented"