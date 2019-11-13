(* Problem 1 *)
let rec subset a b = match a with 
	|[] -> true
	|_ -> if List.exists(fun x -> x = List.hd a) b then subset (List.tl a) b
			else false;;

(* Problem 2 *)
let equal_sets a b = 
	if subset a b && subset b a then true
	else false;;

(* Problem 3 *)
let rec set_union a b = match a with 
	|[] -> b
	|_ -> if not (List.exists(fun x -> x = List.hd a) b) then set_union (List.tl a) (List.hd a :: b)
		else set_union (List.tl a) b;;

(* Problem 4 *)
let rec set_intersection a b = match a with
	|[] -> []
	|_ -> if List.exists (fun x -> x = List.hd a) b then List.hd a :: set_intersection (List.tl a) b
		else set_intersection (List.tl a) b;;

(* Problem 5 *)
let rec set_diff a b = match a with
	|[] -> []
	|_ -> if not (List.exists (fun x -> x = List.hd a) b) then List.hd a :: set_diff (List.tl a) b
		else set_diff (List.tl a) b;; 

(* Problem 6 *)
let rec computed_fixed_point eq f x = 
	if eq (f x) x then x
	else computed_fixed_point eq f (f x);;

(* Problem 7 *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Finds tuples in the rules list that have the matching type *)
let rec find_in_list type_param rules_list = match rules_list with
	|[] -> []
	|hd::tl -> if (fst hd) = type_param then (snd hd) :: find_in_list type_param tl
				else find_in_list type_param tl;;

(* removes duplicates *)
let rec remove_dups list1 = match list1 with
	|[] -> []
	|hd::tl -> if List.exists (fun x -> x = hd) tl then remove_dups tl
				else hd :: remove_dups tl;;

(* adds the rule to the final list *)
let rec add_to_list type_param rules_list = match rules_list with
	|[] -> []
	|hd::tl -> if (fst hd) = type_param then hd :: add_to_list type_param tl
				else add_to_list type_param tl;;

(* remove the terminal values *)
let rec remove_terminal term_list = match term_list with
	|[] -> []
	|hd::tl -> 
		match hd with
		|N sym -> sym :: remove_terminal tl
		|T _ -> remove_terminal tl;;

(* add the appropriate rules based on the give list of nt/t types *)
let rec recurse nt_list rules_list = match nt_list with
	|[] -> []
	|hd::tl -> List.append (recurse tl rules_list) (add_to_list hd rules_list);;

(* orders the list *)
let rec order_list orig_rules curr_rules = match orig_rules with
	|[] -> []
	|hd::tl -> if List.exists (fun x -> x = hd) curr_rules then hd :: order_list tl curr_rules
				else order_list tl curr_rules;;

(* gets the immediate next term list *)
let get_next_list term_type rules = remove_terminal (remove_dups (List.flatten (find_in_list term_type rules)));;

(* gets the appropriate rules based on the list *)
let get_rules rules next_list = order_list rules (recurse (next_list) rules);;

(* gets all the reachable rules *)
let rec get_next_reachable rules reachable_list = match rules with
	|[] -> reachable_list
	|hd::tl -> 
		if List.exists (fun x -> x = (fst hd)) reachable_list then 
			let next_list = get_next_list (fst hd) rules in
			let new_reachable_list = set_union reachable_list next_list in
			get_next_reachable tl new_reachable_list 
		else get_next_reachable tl reachable_list;;

let filter_reachable g = 
	let term_list = (fst g) :: get_next_reachable (snd g) (get_next_list (fst g) (snd g)) in
	(fst g), get_rules (snd g) term_list;;




