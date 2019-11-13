(* Problem 1 Test Cases *)
let subset_test0 = subset [] []
let subset_test1 = subset [] [1;2;3]
let subset_test2 = subset [1;2] [0;1;2;3;4]
let subset_test3 = subset [2;1] [0;1;2;3;4]
let subset_test4 = subset [1;1] [0;1;2;3;4]
let subset_test5 = subset [1;1;3] [0;1;2;3;4]
let subset_test6 = subset [1;1;4;4] [0;1;2;3;4]
let subset_test7 = subset [0;1;2;3;4] [0;1;2;3;4]
let subset_test8 = not (subset [9] [1;2;3])
let subset_test9 = not (subset [1;9] [1;2;3])
let subset_test10 = not (subset [1] [])

(* Problem 2 Test Cases *)
let equal_sets_test0 = equal_sets [] []
let equal_sets_test1 = equal_sets [1;2;3] [1;2;3]
let equal_sets_test2 = equal_sets [1;1;2] [1;2;1]
let equal_sets_test3 = equal_sets [1;1;2] [1;2]
let equal_sets_test4 = equal_sets [1;1;2;2] [1;2]
let equal_sets_test5 = not (equal_sets [1] []) 
let equal_sets_test6 = not (equal_sets [] [1])
let equal_sets_test7 = not (equal_sets [1;2;3] [4;5;6])
let equal_sets_test8 = not (equal_sets [1;2;3;4] [1;2;3;5])


(* Problem 3 Test Cases *)
let set_union_test0 = equal_sets(set_union [] []) []
let set_union_test1 = equal_sets(set_union [1;1] [1;1]) [1]
let set_union_test2 = equal_sets(set_union [1;1] [1;1]) [1;1]
let set_union_test3 = equal_sets(set_union [1;2] [2;3]) [1;2;3]
let set_union_test4 = equal_sets(set_union [1;2] []) [1;2]
let set_union_test5 = equal_sets(set_union [] [1;2]) [1;2]
let set_union_test6 = equal_sets(set_union [1;2] [3;4]) [1;2;3;4]

(* Problem 4 Test Cases *)
let set_intersection_test0 = equal_sets(set_intersection [] []) []
let set_intersection_test1 = equal_sets(set_intersection [1;2;3] []) []
let set_intersection_test2 = equal_sets(set_intersection [] [1;2;3]) []
let set_intersection_test3 = equal_sets(set_intersection [1;2;3] [2]) [2]
let set_intersection_test4 = equal_sets(set_intersection [1;2;3] [2;3;4]) [2;3]
let set_intersection_test5 = equal_sets(set_intersection [1;2;3] [1;2;3]) [1;2;3]

(* Problem 5 Test Cases *)
let set_diff_test0 = equal_sets(set_diff [] []) []
let set_diff_test1 = equal_sets(set_diff [1;2;3] []) [1;2;3]
let set_diff_test2 = equal_sets(set_diff [1;2;3] [3]) [1;2]
let set_diff_test3 = equal_sets(set_diff [1;2;3] [2;3]) [1]
let set_diff_test4 = equal_sets(set_diff [1;2;3] [2;3;4;5]) [1]
let set_diff_test5 = equal_sets(set_diff [] [1;2;3]) []
let set_diff_test6 = equal_sets(set_diff [1] [1;2;3]) []
let set_diff_test7 = equal_sets(set_diff [1;2] [1;2;3]) []

(* Problem 6 Test Cases *)
let computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x) 10 = 10
let computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x / 3) 1000000000 = 0
let computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity
let computed_fixed_point_test3 = computed_fixed_point (=) (fun x -> x *. x)  2. = infinity
let computed_fixed_point_test4 = computed_fixed_point (=) sqrt 1000000. = 1.
let computed_fixed_point_test5 = computed_fixed_point (<) (fun x -> x / 2) 10 = 10
let computed_fixed_point_test6 = computed_fixed_point (>) (fun x -> x + 1) 10 = 10

(* Problem 7 Test Cases *)
type fr_nonterminals = | A | B | C | D | E
let fr_rules = 
	[	
		A, [N E];
		A, [N B];
		B, [N C];
		C, [N D];
		D, [T"random"];
		E, [T"random2"]
	]

let fr_test0 = filter_reachable (A, fr_rules) = (A, fr_rules)
let fr_test1 = filter_reachable (B, fr_rules) = (B, [	
														B, [N C]; 
														C, [N D]; 
														D, [T"random"]
													])
let fr_test2 = filter_reachable (D, fr_rules) = (D, [
														D, [T"random"]
													])
let fr_test3 = filter_reachable (B, List.tl fr_rules) = (B, [
																B, [N C];
																C, [N D];
																D, [T"random"]
															])
let fr_test4 = filter_reachable (A, List.tl fr_rules) = (A, [
																A, [N B];
																B, [N C];
																C, [N D];
																D, [T"random"]
															])