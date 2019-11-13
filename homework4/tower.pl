/* Problem 1 */
/* Helper functions for Problem 1 */
/* checks that all elements in each row and column are unique */
check_unique(N, Matrix) :-
	check_unique_row(N, Matrix),
	check_unique_col(N, Matrix).

check_unique_row(_, []).
check_unique_row(N, [H|T]) :-
	fd_domain(H, 1, N),
	fd_all_different(H),
	check_unique_row(N, T).

check_unique_col(N, Matrix) :-
	transpose(Matrix, MatrixTrans),
	check_unique_row(N, MatrixTrans).

/* transposes a matrix, implementation from SWI Prolog */
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

/* verifies the dimensions of the matrix */
check_lengths(N, Matrix) :-
	length(Matrix, N),
	check_lengths_helper(Matrix, N).

check_lengths_helper([], _).
check_lengths_helper([H|T], N) :-
	length(H, N),
	check_lengths_helper(T, N).

/* checks to see if the row or column satisfies the constraints */
check_bounds([], [], []).
check_bounds([HRow|TRow], [HLeft|TLeft], [HRight|TRight]) :-
	visible(HRow, 0, 0, HLeft),
	reverse(HRow, Rev),
	visible(Rev, 0, 0, HRight),
	check_bounds(TRow, TLeft, TRight).

/* determines how many towers are visible from the left side */
visible([], _, Num, Res) :- Res is Num.
visible([H|T], Max, Num, Res) :-
	H #> Max,
	X is Num + 1,
	visible(T, H, X, Res).
visible([H|T], Max, Num, Res) :-
	H #=< Max,
	visible(T, Max, Num, Res).

/* main predicates for Problem 1 */
tower(N, T, counts(Top, Bottom, Left, Right)) :-
	check_lengths(N, T),
	check_unique(N, T),
	maplist(fd_labeling, T),
	check_bounds(T, Left, Right),
	transpose(T, TTrans),
	check_bounds(TTrans, Top, Bottom).


/* Problem 2 */
/* Helper functions for Problem 2 */
/* removes the first instance of an element in a list */
remove(E, [E|T], T).
remove(E, [H|TIn], [H|TRes]) :-
	E \= H,
	remove(E, TIn, TRes).

/* checks height of matrix and calls other rules */
check_constraints(N, Matrix, counts(Top, Bottom, Left, Right)) :-
	length(Matrix, N),	
	generate_domain(1, N, Domain),
	check_unique_and_domain(N, Domain, Matrix, Left, Right),
	transpose(Matrix, MatrixTrans),
	check_unique_and_domain(N, Domain, MatrixTrans, Top, Bottom).

/* checks width of a particular row, and whether the row satisfies the two bounds */
check_unique_and_domain(_, _, [], [], []).
check_unique_and_domain(N, Domain, [H|T], [HStart|TStart], [HEnd|TEnd]) :-
	length(H, N),
	check_row(Domain, [], H),
	visible_plain(H, 0, 0, HStart),
	reverse(H, HRev),
	visible_plain(HRev, 0, 0, HEnd),
	check_unique_and_domain(N, Domain, T, TStart, TEnd).

/* checks the row to see if they are all unique and if they belong in the domain */
check_row(_, _, []).
check_row(Domain, Prev, [H|T]) :-
	member(H, Domain),
	remove(H, Domain, NewDomain),
	check_row(NewDomain, [H|Prev], T).

/* generates a list from Min to Max inclusive */
generate_domain(Min, Max, [Max|[]]) :- Min is Max.
generate_domain(Min, Max, [Min|T]) :-
	Min =< Max,
	X is Min + 1,
	generate_domain(X, Max, T).

/* same as visible in above, but without fd */
visible_plain([], _, Num, Res) :- Res is Num.
visible_plain([H|T], Max, Num, Res) :-
	H > Max,
	X is Num + 1,
	visible_plain(T, H, X, Res).
visible_plain([H|T], Max, Num, Res) :-
	H =< Max,
	visible_plain(T, Max, Num, Res).


/* main predicates for Problem 2 */
plain_tower(0, T, C) :- T = [[]], C = counts([],[],[],[]).
plain_tower(N, T, C) :-
	check_constraints(N, T, C).
	


/* predicates for speedup */
extract_head([H|_], Res) :-
	Res is H.

tower_test(Res) :-
	statistics(cpu_time, _),
	tower(5, _, counts([2,2,2,2,1], [4,3,2,1,5], [5,1,2,3,4], [1,2,2,2,2])),
	statistics(cpu_time, [_|T1]),
	extract_head(T1, Snd1),
	Res is Snd1.

plain_tower_test(Res) :-
	statistics(cpu_time, _),
	plain_tower(5, _, counts([2,2,2,2,1], [4,3,2,1,5], [5,1,2,3,4], [1,2,2,2,2])),
	statistics(cpu_time, [_|T2]),
	extract_head(T2, Snd2),
	Res is Snd2.

speedup(Res) :-
	tower_test(TTRes),
	plain_tower_test(PTTRes),
	Res is PTTRes / TTRes.


/* Problem 3 */
ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.