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



  
let command_tests =
  [
  ]

let course_tests =
  [
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
