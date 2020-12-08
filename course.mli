(*********************************************************************)
(** 
   Representation of course data.

   This module represents the data stored in course files, including
   the holes and terrain.  It handles loading of that data from JSON as well
   as querying the data.
*)
(*********************************************************************)

(** The abstract type representing the course. *)
type t

(** The abstract type representing a single hole. *)
type hole

(** The type of hole identifiers. *)
type hole_number = int

(** The type of hole location *)
type hole_location = float * float

(** The type of terrain obstacles *)
type terrain

(** The type of wind representing the strength and direction *)
type wind = int * float

(** Raised when an unknown hole is encountered. *)
exception UnknownHole of hole_number


(** [from_json j] is the golf course that [j] represents.
    Requires: [j] is a valid JSON golf course representation. *)
val from_json : Yojson.Basic.t -> t

(** [start_hole c] is the identifier of the starting hole in course [c]. *)
val start_hole : t -> hole_number

(** [num_holes c] is the number of holes in the course [c]. *)
val num_holes : t -> int

(** [get_holes c] returns an array of holes in course [c] *)
val get_holes: t -> hole array

(** [get_hole c hole_num] returns the hole that corresponds to a 
    specific hole number *)
val get_hole: t -> hole_number -> hole

(** [get_hole_loc h] is the coordinate location of hole [h]. 
    Raises: UnknownHole h if h is not a hole number in course [t]*)
val get_hole_loc : t -> hole_number -> hole_location

(** [get_hole_number h] returns the number of hole [h] *)
val get_hole_number: hole -> int

(** [get_par c h] returns the par of hole with hole_number [h] in course [c] 
    Raises: UnknownHole h if h is not a hole number in course [t]*)
val get_par: t -> hole_number -> int

(** [get_hole_par h] returns the par of hole [h] 
    Raises: UnknownHole h if h is not a hole number in course [t]*)
val get_hole_par: hole -> int

(** [difficulty c] is a representation of how difficult course [c] is. *)
val difficulty : t -> string

(** [description c h] is the description of hole [h] in course [c]. 
    Raises: [UnknownHole h] if [h] is not a hole identifier in [c]. *)
val description : t -> hole_number -> string

(** [weather] randomly generates the current wind *)
val wind : unit -> wind

