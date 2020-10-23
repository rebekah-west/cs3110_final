open Course
open Player
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
  course: Course.t;
  current_hole: Course.hole_number;
  curent_player: Player.t;
  scores: scorecard;
}

let init_game players = 
  failwith "Unimplemented"

let current_hole game = game.current_hole

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


let get_course game = game.course

let update_location swing game = 
  let hole_num = current_hole game in 
  let hole_loc = Course.get_hole_loc (get_course game) hole_num in
  let player_loc = Player.get_player_location game.current_player in
  failwith "Unimplemented"
