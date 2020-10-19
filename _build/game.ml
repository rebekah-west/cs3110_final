(*****************************************************)
(* Implementations of game and it's functions*)
(*****************************************************)

(* only need to keep track of score per hole since Player.t
   keeps track of the overall score of a player*)
type hole_score = {
  hole: Course.hole_number;
  player: Player.t;
  hole_score: int;
}

type scorecard = hole_score list list

type t = {
  roster: Player.t list;
  scores: scorecard;
}

let init_game players = 
  failwith "Unimplemented"

let current_hole game = 
  failwith "Unimplemented"

let played game = 
  failwith "Unimplemented"

let current_turn game = 
  failwith "Unimplemented"

let update_turn game = 
  failwith "Unimplemented"

let current_score game = 
  failwith "Unimplemented"

let winner_of_hole game = 
  failwith "Unimplemented"

let winner_of_game game = 
  failwith "Unimplemented"
