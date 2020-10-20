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


let club_parser_helper
    (name : string)
    (input_club : string)
    (expected_output : club) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (parse_club input_club))

let club_parser_exn_helper
    (name : string)
    (input_club : string)
    (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises ~printer:(pp_list pp_string)
               expected_output (fun () -> parse_club input_club))

let angle_parser_helper
    (name : string)
    (input_angle : float)
    (expected_output : angle) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (parse_angle input_angle))

let angle_parser_exn_helper
    (name : string)
    (input_angle : float)
    (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises ~printer:(pp_list pp_string)
               expected_output (fun () -> parse_angle input_angle))

let power_parser_helper
    (name : string)
    (input_power : float)
    (expected_output : power) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (parse_power input_power))

let power_parser_exn_helper
    (name : string)
    (input_power : float)
    (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises ~printer:(pp_list pp_string)
               expected_output (fun () -> parse_power input_power))

let alignment_parser_helper
    (name : string)
    (input_alignment : float)
    (expected_output : alignment) : test = 
  name >:: (fun _ -> assert_equal ~printer:(pp_list pp_string)
               expected_output (parse_power input_power))

let alignment_parser_exn_helper
    (name : string)
    (input_alignment : float)
    (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises ~printer:(pp_list pp_string)
               expected_output (fun () -> parse_alignment input_alignment))

let command_tests =
  [
    club_parser_helper 
      "Testing to ensure that driver will be parsed to a Driver club"
      (parse_club "driver") Driver;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that Driver will be 
    parsed to a Driver club"
      (parse_club "Driver") Driver;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that DriVEr will be 
    parsed to a Driver club"
      (parse_club "DriVEr") Driver;

    club_parser_helper 
      "Testing to ensure that nine iron will be parsed to a NineIron club"
      (parse_club "nine iron") NineIron;
    club_parser_helper 
      "Capitalization shouldn't matter, testing to ensure that Nine iron will 
      be parsed to a NineIron club"
      (parse_club "Nine iron") NineIron;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that nine Iron will 
      be parsed to a NineIron club"
      (parse_club "nine Iron") NineIron;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that Nine Iron will 
      be parsed to a NineIron club"
      (parse_club "Nine Iron") NineIron;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that NiNE IrOn will 
      be parsed to a NineIron club"
      (parse_club "NiNE IrOn") NineIron;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that NINE IRON will 
      be parsed to a NineIron club"
      (parse_club "NINE IRON") NineIron;

    club_parser_helper 
      "Testing to ensure that eight iron will be parsed to a EightIron club"
      (parse_club "eight iron") EightIron;
    club_parser_helper 
      "Capitalization shouldn't matter, testing to ensure that Eight iron will 
      be parsed to a Eight Iron club"
      (parse_club "Eight iron") EightIron;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that eight Iron will 
      be parsed to a EightIron club"
      (parse_club "eight Iron") EightIron;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that Eight Iron will 
      be parsed to a EightIron club"
      (parse_club "Eight Iron") EightIron;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that EighT IrOn will 
      be parsed to a EightIron club"
      (parse_club "EighT IrOn") EightIron;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that EIGHT IRON will 
      be parsed to a EightIron club"
      (parse_club "EIGHT IRON") EightIron;

    club_parser_helper 
      "Testing to ensure that putter will be parsed to a Putter club"
      (parse_club "putter") Putter;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that Putter will be 
    parsed to a Putter club"
      (parse_club "Putter") Putter;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that PuTTeR will be 
    parsed to a Putter club"
      (parse_club "PuTTeR") Putter;

    club_parser_helper 
      "Testing to ensure that pitching wedge will be parsed to a 
      PitchingWedge club"
      (parse_club "pitching wedge") PitchingWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, testing to ensure that 
      Pitching wedge will be parsed to a PitchingWedge club"
      (parse_club "Pitching wedge") PitchingWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that 
      pitching Wedge will be parsed to a PitchingWedge club"
      (parse_club "pitching Wedge") PitchingWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that 
      Pitching Wedge will be parsed to a PitchingWedge club"
      (parse_club "Pitching Wedge") PitchingWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that 
    PitCHiNg weDGe will be parsed to a PitchingWedge club"
      (parse_club "PitCHiNg weDGe") PitchingWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that 
    PITCHING WEDGE will be parsed to a PitchingWedge club"
      (parse_club "PITCHING WEDGE") PitchingWedge;

    club_parser_helper 
      "Testing to ensure that sand wedge will be parsed to a 
      SandWedge club"
      (parse_club "sand wedge") SandWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, testing to ensure that 
      Sand wedge will be parsed to a SandWedge club"
      (parse_club "Sand wedge") SandWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that 
      sand Wedge will be parsed to a SandWedge club"
      (parse_club "sand Wedge") SandWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that 
      Sand Wedge will be parsed to a SandWedge club"
      (parse_club "Sand Wedge") SandWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that 
    sAnD weDGe will be parsed to a SandWedge club"
      (parse_club "sAnD weDGe") SandWedge;
    club_parser_helper 
      "Capitalization shouldn't matter, Testing to ensure that 
    PITCHING WEDGE will be parsed to a SandWedge club"
      (parse_club "SAND WEDGE") SandWedge;

    club_parser_exn_helper "Testing that if an empty string is given an
    [Empty] exception is thrown" 
      (parse_club "") Empty;

    club_parser_exn_helper "Testing that if a mispelled club is given, a 
    [Malformed] exception is thrown"
      (parse_club "Drvier") Malformed;

    club_parser_exn_helper "Testing that if something other than a string is
      given, an [InvalidArgument] exception is thrown"
      (parse_club 27) Invalid_argument

      power_parser_helper "Testing to ensure a valid float input for power
     parses correctly"
      (parse_power 50.0) 50.0;
    power_parser_helper "Testing to ensure a valid float input for power
     parses correctly, edge case"
      (parse_power 0.0) 0.0;
    power_parser_helper "Testing to ensure a valid float input for power
     parses correctly, edge case"
      (parse_power 1.0) 1.0;
    power_parser_helper "Testing to ensure a valid float input for power
     parses correctly, edge case"
      (parse_power 99.0) 99.0;
    power_parser_helper "Testing to ensure a valid float input for power
     parses correctly, edge case"
      (parse_power 100.0) 100.0;
    power_parser_helper "Testing to ensure a valid float input for power
     parses correctly, atypical input"
      (parse_power 21.387) 21.387;

    power_parser_exn_helper "Testing to ensure an [Empty] exception is thrown
     if no argument is given"
      (parse_power ) Empty;
    power_parser_exn_helper "Testing to ensure an [InvalidArgument] exception 
     is thrown if the incorrect type is given"
      (parse_power "five") InvalidArgument;
    power_parser_exn_helper "Testing to ensure a [ValueOutOfRange] exception
      is thrown if the user gives a value outside of the acceptable range"
      (parse_power 150.0) ValueOutOfRange;
    power_parser_exn_helper "Testing to ensure a [ValueOutOfRange] exception
      is thrown if the user gives a value outside of the acceptable range"
      (parse_power -50.0) ValueOutOfRange;


    angle_parser_helper "Testing to ensure a valid float input for angle
     parses correctly"
      (parse_angle 45.0) 45.0;
    angle_parser_helper "Testing to ensure a valid float input for angle
     parses correctly, edge case"
      (parse_angle 0.0) 0.0;
    angle_parser_helper "Testing to ensure a valid float input for angle
     parses correctly, edge case"
      (parse_angle 1.0) 1.0;
    angle_parser_helper "Testing to ensure a valid float input for angle
     parses correctly, edge case"
      (parse_angle 89.0) 89.0;
    angle_parser_helper "Testing to ensure a valid float input for angle
     parses correctly, edge case"
      (parse_angle 90.0) 90.0;
    angle_parser_helper "Testing to ensure a valid float input for angle
     parses correctly, atypical input"
      (parse_angle 21.387) 21.387;

    angle_parser_exn_helper "Testing to ensure an [Empty] exception is thrown
     if no argument is given"
      (parse_angle ) Empty;
    angle_parser_exn_helper "Testing to ensure an [InvalidArgument] exception 
     is thrown if the incorrect type is given"
      (parse_power "five") InvalidArgument;
    angle_parser_exn_helper "Testing to ensure a [ValueOutOfRange] exception
      is thrown if the user gives a value outside of the acceptable range"
      (parse_angle 150.0) ValueOutOfRange;
    angle_parser_exn_helper "Testing to ensure a [ValueOutOfRange] exception
      is thrown if the user gives a value outside of the acceptable range"
      (parse_angle -50.0) ValueOutOfRange;

    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly"
      (parse_alignment 5.0) 5.0;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly"
      (parse_alignment 5.0) Right;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly"
      (parse_alignment -5.0) -5.0;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly"
      (parse_alignment -5.0) Left;

    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment 1.0) 1.0;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment 1.0) Right;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment -1.0) -1.0;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment -1.0) Left;

    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment 0.0) 0.0;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment 0.0) Right;


    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment 90.0) 90.0;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment 90.0) Right;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment -90.0) -90.0;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment -90.0) Left;


    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment 89.0) 89.0;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment 89.0) Right;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment -89.0) -89.0;
    alignment_parser_helper "Testing to ensure a valid float input for 
    alignment parses corresctly, edge case"
      (parse_alignment -89.0) Left;

    alignment_parser_exn_helper "Testing to ensure the [Empty] exception
    is thrown if no argument is given"
      (parse_alignment ) Empty
      alignment_parser_exn_helper "Testing to ensure the [InvalidInput] exception
    is thrown if a type other than float is given"
      (parse_alignment "left 20 degrees") InvalidArgument
      alignment_parser_exn_helper "Testing to ensure the [ValueOutOfRange]
    exception is thrown if a value is given outside of the range of acceptable
    values"
      (parse_alignment -110.0) ValueOutOfRange
      alignment_parser_exn_helper "Testing to ensure the [ValueOutOfRange]
    exception is thrown if a value is given outside of the range of acceptable
    values"
      (parse_alignment 110.0) ValueOutOfRange
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
    current_hole_test "Ensure the game starts at hole 1"
      (current_hole initialized_game) 1;

    played_test "Ensure the game starts with no holes played"
      (played initialized_game) [];

    current_turn_test "Ensure the game starts the 1st hole with the
    player who was first in the lineup"
      (current_turn initialized_game) first_player
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


