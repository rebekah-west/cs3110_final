open OUnit2
open Player
open Command
open Game
open Course


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

let course_tests =
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


let game_tests =
  [
  ]

let player_tests =
  [
  ]

let suite =
  "test suite for final project"  >::: List.flatten [
    command_tests;
    course_tests;
    game_tests;
    player_tests;
  ]

let _ = run_test_tt_main suite
