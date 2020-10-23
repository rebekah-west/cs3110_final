(** AF:
    RI:
*)
type hole_number = int
type hole_location = int * int


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
  hole_location: hole_location;
  description: string;
  terrain: terrain list
}


(** AF:
    RI:
*)
type t = {
  holes: hole list;
  difficulty: string;
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

let get_holes course = course.holes

let rec get_hole hole_list hole_number = 
  match hole_list with 
  | [] -> raise (UnknownHole hole_number)
  | hole::rest_of_holes -> begin
      if hole.hole_number = hole_number then hole 
      else get_hole rest_of_holes hole_number
    end

let get_hole_loc course hole_number =
  let hole = get_hole course.holes hole_number in 
  hole.hole_location

let difficulty course = course.difficulty

let description course hole_number =
  let hole = get_hole course.holes hole_number in 
  hole.description

let wind () = 
  failwith "unimplemented"

let get_hole_number hole = hole.hole_number