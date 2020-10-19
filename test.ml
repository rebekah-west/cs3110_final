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

let command_tests =
  [
  ]

(* make the course t object *)
let robert_trent = Course.from_json (Yojson.Basic.from_file "RobertTrent.json")

(* course testing helper functions  *)
let start_hole_test (name: string) (course:Course.t) (output:hole_number) = 
  name >:: (fun _ -> assert_equal output (start_hole course))

let num_holes_test (name: string) (course:Course.t) (output:int) = 
  name >:: (fun _ -> assert_equal output (num_holes course))

let difficulty_test (name: string) (course:Course.t) (output:string) = 
  name >:: (fun _ -> assert_equal output (difficulty course))

let description_test (name: string) (course:Course.t) (num:hole_num) 
    (output:string) = 
  name >:: (fun _ -> assert_equal output (description course num))

let description_exceptions_test (name: string) (course:Course.t) (num:hole_num) 
    (output:string) = 
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

let game_tests =
  [
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

let players = Player.read_players from_json(Yojson.Basic.from_file("Players.json"))
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
