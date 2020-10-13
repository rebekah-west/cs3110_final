(** The abstract type of values representing courses. *)
type t = list

(** The type of hole identifiers. *)
type hole_number = int

(** The type of terrain obstacles *)
type terrain =
| Lake
| Tree
| Sand

(** Raised when an unknown hole is encountered. *)
exception UnknownHole of hole_number


(** [from_json j] is the golf course that [j] represents.
    Requires: [j] is a valid JSON golf course representation. *)
let from_json j =
  failwith "unimplemented"


(** [start_hole c] is the identifier of the starting hole in course [c]. *)
let start_hole = 
  failwith "unimplemented"

(** [num_holes c] is the number of holes in the course [c]. *)
let num_holes =
  failwith "unimplemented"

(** [difficulty c] is a representation of how difficult course [c] is. *)
let difficulty =
  failwith "unimplemented"

(** [description c h] is the description of hole [h] in course [c]. 
    Raises [UnknownHole h] if [h] is not a hole identifier in [c]. *)
let description =
  failwith "unimplemented"