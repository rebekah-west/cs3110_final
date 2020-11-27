open OUnit2
open Command
open Course
open Game
open Player
open Str

let () = Printexc.record_backtrace true

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_int (k,v)] pretty-prints the tuple [(k,v)]. *)
let pp_tup (k,v) = "(" ^ string_of_float k ^ ", " ^ string_of_float v ^ ")"
(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let pp_player pl = 
  let name = get_player_name pl in 
  pp_string name

(* A helper function to test parse_club by comparing expected output to 
   actual output*)
let club_parser_helper(name : string) (input_club : string)
    (expected_output : Command.club) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse_club input_club))

let angle_parser_helper(name : string)(input_angle : int)
    (expected_output : Command.angle) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse_angle input_angle))

let alignment_parser_helper(name : string)(input : int)
    (expected_output : Command.alignment) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse_alignment input))

let power_parser_helper(name : string)(input_power : int)
    (expected_output : Command.power) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse_power input_power))

(* A helper function to test parse_swing by seeing if proper exceptions are
   thrown at run time*)
let swing_parser_exn_helper
    (name : string) (input_power : int) (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> Command.parse_swing ()))


let command_tests =
  [ club_parser_helper "'driver' to a Driver club" "driver" Driver;
    club_parser_helper  "'Driver' to a Driver club, first letter 
      capitalization" "Driver" Driver;
    club_parser_helper  "'DriVEr' to a Driver club, random 
      capitalization" "DriVEr" Driver;
    club_parser_helper  "'DRIVER' to a Driver club, full
      capitalization" "DriVEr" Driver;

    club_parser_helper " 'nine iron' to a NineIron club" "nine iron" NineIron;
    club_parser_helper " 'Nine iron' to a NineIron club, first word 
      capitalization" "Nine iron" NineIron;
    club_parser_helper " 'nine Iron' to a NineIron club, second word 
      capitalization" "nine Iron" NineIron;
    club_parser_helper " 'Nine Iron' to a NineIron club, both words
      capitalized" "Nine Iron" NineIron;
    club_parser_helper " 'NiNE IrOn' to a NineIron club, random
      capitalization" "NiNE IrOn" NineIron;
    club_parser_helper " 'NINE IRON' to a NineIron club, all
      capitalization" "NINE IRON" NineIron;

    club_parser_helper "'eight iron' to an EightIron club" 
      "eight iron" EightIron;
    club_parser_helper "'Eight iron' to an EightIron club, first word
      capitalization" "Eight iron" EightIron;
    club_parser_helper "'eight Iron' to an EightIron club, second word
      capitalization" "eight Iron" EightIron;
    club_parser_helper "'Eight Iron' to an EightIron club, both word
      capitalization" "Eight Iron" EightIron;
    club_parser_helper "'EiGHt IrOn' to an EightIron club, random
      capitalization" "EiGHt IrOn" EightIron;
    club_parser_helper "'EIGHT IRON' to an EightIron club, full
      capitalization" "EIGHT IRON" EightIron;

    club_parser_helper "'putter' to a Putter club" "putter" Putter;
    club_parser_helper "'Putter' to a Putter club, 1st letter capitalization" 
      "Putter" Putter;
    club_parser_helper "'PutTEr' to a Putter club, 1st letter capitalization" 
      "PutTEr" Putter;
    club_parser_helper "'PUTTER' to a Putter club, full capitalization" 
      "PUTTER"  Putter;


    club_parser_helper "'pitching wedge' to a PitchingWedge club" 
      "pitching wedge" PitchingWedge;
    club_parser_helper "'Pitching wedge' to a PitchingWedge club, first word
      capitalization" "Pitching wedge" PitchingWedge;
    club_parser_helper "'pitching Wedge' to a PitchingWedge club, second word
      capitalization" "pitching Wedge" PitchingWedge;
    club_parser_helper "'Pitching Wedge' to a PitchingWedge club, both word
      capitalization" "Pitching Wedge" PitchingWedge;
    club_parser_helper "'PiTChiNg WedgE' to a PitchingWedge club, random
      capitalization" "PiTChiNg WedgE" PitchingWedge;
    club_parser_helper "'PITCHING WEDGE' to a PitchingWedge club, random
      capitalization" "PITCHING WEDGE" PitchingWedge;

    club_parser_helper "'sand wedge' to a SandWedge club" "sand wedge"
      SandWedge;
    club_parser_helper "'Sand wedge' to a SandWedge club, first word 
      capitalization" "Sand wedge" SandWedge;
    club_parser_helper "'sand Wedge' to a SandWedge club, second word 
      capitalization" "sand Wedge" SandWedge;
    club_parser_helper "'Sand Wedge' to a SandWedge club, both word 
      capitalization" "Sand Wedge" SandWedge;
    club_parser_helper "'SanD wEDge' to a SandWedge club, random 
      capitalization" "SanD wEDge" SandWedge;
    club_parser_helper "'SAND WEDGE' to a SandWedge club, full
      capitalization" "SAND WEDGE" SandWedge;

    power_parser_helper "int 50 to power 50" 50 50;
    power_parser_helper "int 21 to power 21" 21 21;
    power_parser_helper "int 0 to power 0, edge case" 0 0;
    power_parser_helper " int 1 to power 1, edge case" 1 1;
    power_parser_helper "int 99 to power 99" 99 99;
    power_parser_helper "int 100 to power 100" 100 100;

    angle_parser_helper "int 45 to angle 45" 45 45;
    angle_parser_helper "int 21 to angle 21" 21 21;
    angle_parser_helper "int 0 to angle 0, edge case" 0 0;
    angle_parser_helper "int 1 to angle 1, edge case" 1 1;
    angle_parser_helper "int 89 to angle 89, edge case" 89 89;
    angle_parser_helper "int 90 to angle 90, edge case" 90 90;

    alignment_parser_helper "int 5 to alignment 5" 5 5;
    alignment_parser_helper "int -5 to alignment -5" (-5) (-5);

    alignment_parser_helper "int 1 to alignment 1, edge case" 1 1;
    alignment_parser_helper "int -1 to alignment -1, edge case" (-1) (-1);

    alignment_parser_helper "int 0 to alignment 0, edge case" 0 0;

    alignment_parser_helper "int 90 to alignment 90, edge case" 90 90;
    alignment_parser_helper "int -90 to alignment -90, edge case" (-90) (-90);

    alignment_parser_helper "int 89 to alignment 89" 89 89;
    alignment_parser_helper "int -89 to alignment -89" (-89) (-89);
  ]

(* make the course t object *)
let robert_trent = Course.from_json (Yojson.Basic.from_file "RobertTrent.json")
let pebble = Course.from_json (Yojson.Basic.from_file "PebbleBeach.json")

(* course testing helper functions  *)
let start_hole_test (name: string) (course:Course.t) (output:Course.hole_number) 
  = name >:: (fun _ -> assert_equal output (start_hole course))
let num_holes_test (name: string) (course:Course.t) (output:int) = 
  name >:: (fun _ -> assert_equal output (num_holes course))
let holes_array_test (name: string) (course:Course.t) (num_holes:int) = 
  name >:: (fun _ -> assert_equal num_holes (Array.length (get_holes course)))
let hole_loc_test (name: string) (course:Course.t) (hole:Course.hole_number) 
    (output : Course.hole_location)
  = name >:: (fun _ -> assert_equal output (get_hole_loc course hole))
let par_test (name: string) (course:Course.t) (hole:Course.hole_number) 
    (output : int)
  = name >:: (fun _ -> assert_equal output (get_par course hole))
let difficulty_test (name: string) (course:Course.t) (output:string) = 
  name >:: (fun _ -> assert_equal output (difficulty course))
let description_test (name: string) (course:Course.t) (num:Course.hole_number) 
    (output:string) = 
  name >:: (fun _ -> assert_equal output (description course num))
let description_exceptions_test 
    (name: string) (course:Course.t) (num:Course.hole_number) (output:exn) = 
  name >:: (fun _ -> assert_raises output (fun _ ->  description course num))

(* course test suite *)
let course_tests =
  [
    start_hole_test "Robert Trent start hole" robert_trent 1;
    num_holes_test "Robert Trent num_holes is 2" robert_trent 2;
    num_holes_test "Pebble Beach num_holes is 3" pebble 3;
    hole_loc_test "Robert Trent hole 1 at 230,45" robert_trent 1 (230.,45.); 
    hole_loc_test "Pebble Beach hole 1 located at 430,45" pebble 1 (430.,45.); 
    hole_loc_test "Pebble Beach hole 3 located at 530,65" pebble 3 (530.,65.); 
    holes_array_test "Pebble has 3 holes" pebble 3;
    holes_array_test "Trent has 2 holes" robert_trent 2;
    par_test "Trent hole 1 has par 3" robert_trent 1 3;
    par_test "Trent hole 2 has par 4" robert_trent 2 4;
    par_test "Pebble hole 1 has par 5" pebble 1 5;
    difficulty_test "Robert Trend difficulty" robert_trent "easy";
    difficulty_test "Pebble Beach difficulty is hard" pebble "hard";
    description_test "Robert Trend hole 1 desc" robert_trent 1 
      "Welcome to Robert Trent. Enjoy golfing today. The hole has a lake to the southwest.";
    description_test "Robert Trend hole 2 desc" robert_trent 2 
      "This hole is a long drive. Get ready to swing!";
    description_test "Pebble hole 2 desc" pebble 2 
      "Avoid the ocean on this beautiful hole!";
    description_exceptions_test "Robert Trent not there" robert_trent
      76 (UnknownHole 76);
    (* can i even test wind?? not really i think? *)
    (* test that wind is an int *)
  ]

let test_players = Yojson.Basic.from_file "Players.json" |> read_players 
let first_player = test_players.(0)

let test_player_input = Player.init_players ()

let test_course = Yojson.Basic.from_file "RobertTrent.json" |> from_json
let initialized_game = init_game test_players test_course


let player_name_test (name : string) (input : Player.t) (exp_output : string) : 
  test = name >:: (fun _ -> 
    assert_equal exp_output (Player.get_player_name input) ~printer:pp_string)

let pl_pow_test (name : string) (input : Player.t) (exp_output : float) : 
  test = name >:: (fun _ -> assert_equal exp_output ~printer:string_of_float
                      (Player.get_player_power_multiplier input))

let pl_acc_test (name : string) (input : Player.t) (exp_output : float) : 
  test = name >:: (fun _ -> assert_equal exp_output ~printer:string_of_float
                      (Player.get_player_accuracy_multiplier input) )

let pl_handicap_test (name : string) (input : Player.t) (exp_output : int) : 
  test = name >:: (fun _ -> assert_equal exp_output ~printer:string_of_int
                      (Player.get_player_handicap input))

let player_location_test (name : string) (input : Player.t) 
    (exp_output : float*float) : test = name >:: (fun _ -> 
    assert_equal exp_output(Player.get_player_location input))

let dist_from_hole_test (name : string) (player_loc : float * float ) 
    (hole_loc : float * float ) (exp_output : float) : test = 
  name >:: (fun _ -> assert_equal true 
               ( (exp_output -.(Player.dist_from_hole player_loc hole_loc)) 
                 < (0.001)) )

let jenna = test_players.(0)
let gian = test_players.(2)

(*"Robert Trent hole 1 at 230,45"*)
let rt_hole1 = get_hole_loc robert_trent 1
(*"Pebble Beach hole 1 located at 430,45"*)
let pb_hole1 = get_hole_loc pebble 1 

let player_tests =
  [
    player_name_test "Jenna is first from Players.json" jenna "Jenna";
    player_name_test "Gian is third from Players.json" gian "Gian";
    pl_pow_test "Jenna has 1.5 power" jenna 1.5;
    pl_pow_test "Gian has 0.8 power" gian 0.8;
    pl_acc_test "Jenna has 1.0 accuracy" jenna 1.0;
    pl_acc_test "Gian has 0.9 accuracy" gian 0.9;
    pl_handicap_test "Jenna has -5 handicap" jenna (-5);
    pl_handicap_test "Gian has 20 handicap" gian 20;
    player_location_test "Gian starts at 0,0" gian (0.,0.);
    player_location_test "Jenna starts at 0,0" jenna (0.,0.);
    dist_from_hole_test "Hole 1 robert trent" (0.,0.) (230.,45.) 234.361;
    dist_from_hole_test "Hole 1 pebble beach" (0.,0.) (430.,45.) 432.348;
  ]

let current_hole_test (name : string) (input_game : Game.t)
    (expected_output : Course.hole_number) : test = 
  name >:: (fun _ -> assert_equal expected_output (current_hole input_game))

let played_test (name : string)(input_game : Game.t)
    (expected_output : Course.hole_number list) : test = 
  name >:: (fun _ -> assert_equal expected_output (Game.played input_game))

let current_turn_test(name : string)(input_game : Game.t)
    (expected_output : string) : test = 
  name >:: (fun _ -> assert_equal expected_output ~printer:pp_string
               (get_player_name (current_turn input_game)))

let current_turn_valid_player(name : string)(input_game : Game.t)
    (players : Player.t array) : test = name >:: (fun _ -> 
    assert_equal true (Array.mem (current_turn input_game) 
                         players ) )

let current_score_test(name : string)(input_game : Game.t)
    (expected_output : scorecard) : test = 
  name >:: (fun _ -> assert_equal expected_output (current_score input_game))

let winner_of_hole_test(name : string)(input_game : Game.t)
    (input_hole : Course.hole_number )(expected_output : Player.t array) 
  : test = name >:: (fun _ -> assert_equal expected_output 
                        (winner_of_hole input_game input_hole))

let winner_of_game_test(name : string)(input_game : Game.t)
    (expected_output : Player.t array) : test = 
  name >:: (fun _ -> assert_equal expected_output (winner_of_game input_game))
(* let swing2_game = play_one_swing_of_hole swing1_game *)
(* let hole_one_complete_game = play_hole initialized_game
   let hole_two_complete_game = play_hole hole_one_complete_game *)

let initial_game_tests =
  [
    (* tests on the initialization*)
    current_hole_test "The game starts at hole 1" initialized_game 1;
    current_turn_test "Game starts with the player who was first in the lineup"
      initialized_game (get_player_name first_player);
    current_turn_valid_player "Is the player returned by current turn a valid player" 
      initialized_game (game_roster initialized_game);
    played_test "Ensure the game starts with no holes played" 
      initialized_game [];
    (* played_test "Hole 1 added to holes_played after play_hole called once"
       hole_one_complete_game [1];
       played_test "Hole 2 added to holes_played after play_hole called twice"
       hole_two_complete_game [1;2];  *)
  ]

let swing1_game = play_one_swing_of_hole initialized_game

let swing1_game_tests = [
  current_hole_test "The game stays at hole 1" swing1_game 1;
  current_turn_test "Game moves to next player in lineup"
    swing1_game (get_player_name test_players.(2));
  current_turn_valid_player "Is the player updated to a valid player
       after swing one" swing1_game (game_roster swing1_game);
  played_test "played should still be empty after one swing" 
    swing1_game [];
]

let suite =
  "test suite for final project"  >::: List.flatten [
    command_tests;
    course_tests;
    player_tests;
    (* make sure game tests is last  *)
    initial_game_tests;
    swing1_game_tests;
  ]

let _ = run_test_tt_main suite


