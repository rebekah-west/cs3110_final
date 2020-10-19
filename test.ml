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
