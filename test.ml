open OUnit2
open Command
open Course
open Game
open Player

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

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

(* A helper function to test parse_club by comparing expected output to 
   actual output*)
let club_parser_helper
    (name : string) (input_club : string)
    (expected_output : club) : test = 
  name >:: (fun _ -> assert_equal expected_output (parse_club input_club))

(* A helper function to test parse_club by seeing if proper exceptions are
   thrown at run time*)
let club_parser_exn_helper (name : string) (input_club : string)
    (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> parse_club input_club))

(* A helper function to test parse_swing by seeing if proper exceptions are
   thrown at run time*)
let swing_parser_exn_helper
    (name : string) (input_power : int) (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises expected_output 
               (fun () -> parse_swing ()))

(* create some test swings just for reference 
   to see if everything works, testing in utop *)
let swing1 = create_swing "driver" 10 40 3 
(*good*)
let swing2 = create_swing "nine iron" 70 30 6 
(*Exception: Invalid_argument "That is not a club".*)
let swing3 = create_swing "PiTCHINg WeDGe" 70 30 6
(*Exception: Invalid_argument "That is not a club".*)
let swing4 = create_swing "putter" 110 30 6
(*This worked but shouldnt have, power needs to be bounded by 100*)
let swing5 = create_swing "sandwedge" (-10) 30 6
(*This worked but shouldn't have, power needs to be bounded by 0*)
let swing6 = create_swing "eightiron" 75 100 6
(*worked but shouldnt, need to bound angle at 90*)
let swing7 = create_swing "eightiron" 75 (-10) 6
(*worked but shouldn't, need to bound angle at 0*)
let swing8 = create_swing "eightiron" 75 40 (-6)
(*Good*)
let swing9 = create_swing "eightiron" 75 40 (-600)
(*worked but shouldn't, need to bound alignment at -90*)
let swing9 = create_swing "eightiron" 75 40 (600)
(*worked but shouldn't, need to bound alignment at 90*)


let command_tests =
  [
    club_parser_helper "'driver' to a Driver club" "driver" Driver;
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

    club_parser_exn_helper "Empty string is given -> [Empty] exception is 
      thrown" "" Empty;
    club_parser_exn_helper "Mispelled club is given -> [Malformed] exception 
    is thrown" "Drvier" (Invalid_argument "That is not a club");

    (* power_parser_helper "int 50 to power 50" 50 50;
       power_parser_helper "int 21 to power 21" 21 21;
       power_parser_helper "int 0 to power 0, edge case" 0 0;
       power_parser_helper " int 1 to power 1, edge case" 1 1;
       power_parser_helper "int 99 to power 99" 99 99;
       power_parser_helper "int 100 to power 100" 100 100;

       power_parser_exn_helper "ValueOutOfRange] exception is thrown if value 
       above the acceptable range" 150 ValueOutOfRange;
       power_parser_exn_helper "[ValueOutOfRange] exception is thrown if value 
       below the acceptable range" (-50) ValueOutOfRange;

       angle_parser_helper "int 45 to angle 45" 45 45;
       angle_parser_helper "int 21 to angle 21" 21 21;
       angle_parser_helper "int 0 to angle 0, edge case" 0 0;
       angle_parser_helper "int 1 to angle 1, edge case" 1 1;
       angle_parser_helper "int 89 to angle 89, edge case" 89 89;
       angle_parser_helper "int 90 to angle 90, edge case" 90 90;

       angle_parser_exn_helper "ValueOutOfRange] exception thrown for angle 
       above acceptable range" 150 ValueOutOfRange;
       angle_parser_exn_helper "ValueOutOfRange] exception thrown for angle 
       below acceptable range" (-50) ValueOutOfRange;

       alignment_parser_helper "int 5 to alignment 5" 5 5;
       alignment_parser_helper "int -5 to alignment -5" (-5) (-5);

       alignment_parser_helper "int 1 to alignment 1, edge case" 1 1;
       alignment_parser_helper "int -1 to alignment -1, edge case" (-1) (-1);

       alignment_parser_helper "int 0 to alignment 0, edge case" 0 0;

       alignment_parser_helper "int 90 to alignment 90, edge case" 90 90;
       alignment_parser_helper "int -90 to alignment -90, edge case" (-90) (-90);

       alignment_parser_helper "int 89 to alignment 89" 89 89;
       alignment_parser_helper "int -89 to alignment -89" (-89) (-89);


       alignment_parser_exn_helper "[ValueOutOfRange] thrown if a value is 
       above 90" 110 ValueOutOfRange;
       alignment_parser_exn_helper "[ValueOutOfRange] thrown if a value is  
       below -90" (-110) ValueOutOfRange; *)
    swing_parser_exn_helper
  ]

(* make the course t object *)
let robert_trent = Course.from_json (Yojson.Basic.from_file "RobertTrent.json")

(* course testing helper functions  *)
let start_hole_test (name: string) (course:Course.t) (output:Course.hole_number) = 
  name >:: (fun _ -> assert_equal output (start_hole course))

let num_holes_test (name: string) (course:Course.t) (output:int) = 
  name >:: (fun _ -> assert_equal output (num_holes course))

let difficulty_test (name: string) (course:Course.t) (output:string) = 
  name >:: (fun _ -> assert_equal output (difficulty course))

let description_test (name: string) (course:Course.t) (num:Course.hole_number) 
    (output:string) = 
  name >:: (fun _ -> assert_equal output (description course num))

let description_exceptions_test (name: string) (course:Course.t) (num:Course.hole_number) 
    (output:exn) = 
  name >:: (fun _ -> assert_raises output (fun _ ->  description course num))

(* course test suite *)
let course_tests =
  [
    start_hole_test "Robert Trent start hole" robert_trent 1;
    num_holes_test "Robert Trent num_holes" robert_trent 2;
    difficulty_test "Robert Trend difficulty" robert_trent "easy";
    description_test "Robert Trend hole 1 desc" robert_trent 1 
      "Welcome to Robert Trent. Enjoy golfing today. The hole has a lake to the southwest.";
    description_test "Robert Trend hole 2 desc" robert_trent 2 
      "This hole is a long drive. Get ready to swing!";
    description_exceptions_test "Robert Trent not there" robert_trent
      76 (UnknownHole 76);
    (* can i even test wind?? not really i think? *)
    (* test that wind is an int *)

  ]

let current_hole_test (name : string) (input_game : Game.t)
    (expected_output : Course.hole_number) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (current_hole input_game))

let played_test
    (name : string)
    (input_game : Game.t)
    (expected_output : Course.hole_number list) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (current_hole input_game))

let current_turn_test
    (name : string)
    (input_game : Game.t)
    (expected_output : Player.t) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (current_turn input_game))

let current_score_test
    (name : string)
    (input_game : Game.t)
    (expected_output : scorecard) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (current_score input_game))

let winner_of_hole_test
    (name : string)
    (input_game : Game.t)
    (expected_output : Player.t) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (winner_of_hole input_game))

let winner_of_game_test
    (name : string)
    (input_game : Game.t)
    (expected_output : Player.t) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (winner_of_game input_game))

let test_players = Yojson.Basic.from_file "Players.json" |> read_players 
let first_player =
  match test_players with 
  |h :: t -> h
  |[] -> Empty

let test_course = Yojson.Basic.from_file "RobertTrent.json" |> from_json
let initialized_game = init_game test_players test_course

let game_tests =
  [
    (*tests on the initialization*)
    current_hole_test "The game starts at hole 1" initialized_game 1;
    current_turn_test "Game starts with the player who was first in the lineup"
      initialized_game first_player;
    played_test "Ensure the game starts with no holes played" 
      initialized_game [];
  ]

let player_name_test (name : string) (input : Player.t) (exp_output : string) : 
  test = name >:: (fun _ -> 
    assert_equal exp_output (Player.get_player_name input) ~printer:pp_string)

let pl_pow_test (name : string) (input : Player.t) (exp_output : float) : 
  test = name >:: (fun _ -> assert_equal exp_output 
                      (Player.get_player_power_multiplier input) ~printer:string_of_float)

let pl_acc_test (name : string) (input : Player.t) (exp_output : float) : 
  test = name >:: (fun _ -> assert_equal exp_output 
                      (Player.get_player_accuracy_multiplier input) ~printer:string_of_float)

let pl_handicap_test (name : string) (input : Player.t) (exp_output : int) : 
  test = name >:: (fun _ -> assert_equal exp_output 
                      (Player.get_player_handicap input) ~printer:string_of_int)

let players = Player.read_players(Yojson.Basic.from_file("Players.json"))
let jenna = List.hd players
let gian = List.nth players 2

let player_tests =
  [
    player_name_test "Jenna is first from Players.json" jenna "Jenna";
    player_name_test "Gian is third from Players.json" gian "Gian";
    pl_pow_test "Jenna has 1.5 power" jenna 1.5;
    pl_pow_test "Gian has 0.8 power" gian 0.8;
    pl_acc_test "Jenna has 1.0 accuracy" jenna 1.0;
    pl_acc_test "Gian has 0.9 accuracy" gian 0.9;
    pl_handicap_test "Jenna has -5 handicap" jenna ~-5;
    pl_handicap_test "Gian has 20 handicap" gian 20;
  ]

let suite =
  "test suite for final project"  >::: List.flatten [
    command_tests;
    course_tests;
    game_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite


