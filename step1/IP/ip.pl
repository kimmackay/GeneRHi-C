:- use_module(library(lists)).
:- use_module(library(csv)).
:- use_module(library(system3)).

chromosome(pombe, 1, 1, 558).
chromosome(pombe, 2, 559, 1012).
chromosome(pombe, 3, 1013, 1258).
chromosome(elegans, 1, 1, 302).
chromosome(elegans, 2, 303, 608).
chromosome(elegans, 3, 609, 884).
chromosome(elegans, 4, 885, 1234).
chromosome(elegans, 5, 1235, 1653).
chromosome(elegans, 6, 1654, 2008).

solve_ip(Species, Set1, Set2, M) :-
	load_files(Species),
	retractall(edge(_,_,_)),
	(   adjacency(B1, B2, F, Chr1, Chr2),
	    B1 < B2,
	    (   member(Chr1, Set1), member(Chr2, Set2) -> true
	    ;   member(Chr1, Set2), member(Chr2, Set1) -> true
	    ),
	    assertz(edge(B1, B2, F)),
	    fail
	;   true
	),
	% findall(edge(I,J,W), edge(I,J,W), Edges),
	findall(I-J, (edge(I,J,_); edge(J,I,_)), Pairs0),
	keysort(Pairs0, Pairs1),
	keyclumped(Pairs1, Pairs2),
	tell('/tmp/all.lp'),
	write('Maximize\n  obj:'),
	(   foreach(I1-J1,Pairs1),
	    fromto(' ',S1,S2,_)
	do  (   I1>J1 -> S1 = S2
	    ;   edge(I1, J1, F1),
		format('~a~w X_~d_~d', [S1,F1,I1,J1]),
		S2 = ' + '
	    )
	), nl,
	write('Subject To\n'),
	(   foreach(I2-J2s,Pairs2),
	    param(M)
	do  (   length(J2s,Len), Len =< M -> true
	    ;   (   foreach(J2,J2s),
		    fromto(' ',S3,' + ',_),
		    param(I2)
		do  sort2(I2, J2, I3, J3),
		    format('~aX_~d_~d', [S3,I3,J3])
		),
		format(' <= ~d\n', [M])
	    )
	),
	write('Binary\n'),
	(   foreach(I4-J4,Pairs1)
	do  (   I4>J4 -> true
	    ;   format(' X_~d_~d\n', [I4,J4])
	    )
	),
	write('End\n'),
	told,
	system('gurobi_all'),
	parse_solution.

parse_solution :-
	see('/tmp/all.sol'),
	retractall(solution(_,_,_)),
	read_line(_),
	read_line(_),
	repeat,
	  read_line(Codes),
	  (   Codes = end_of_file -> true
	  ;   tok_sol_line(I, J, 1, Codes, []),
	      edge(I, J, W),
	      assertz(solution(I,J,W)),
	      fail
	  ), !,
	seen,
	findall(W, solution(_,_,W), Ws),
	length(Ws, Size),
	sumlist(Ws, Weight),
	format('% Gurobi computed a set of ~d edges and weight ~w\n', [Size,Weight]),
	findall(0, (solution(I,J,W), print_tab([I,J,W])), _).

tok_sol_line(I, J, Z01) --> "X_",
	tok_int(0, I), "_",
	tok_int(0, J), " ",
	tok_int(0, Z01).

tok_int(Int0, Int) --> [D], {D >= 0'0, D =< 0'9}, !,
	{Int1 is 10*Int0 + D - 0'0},
	tok_int(Int1, Int).
tok_int(Int, Int) --> [].

print_tab(L) :-
	(foreach(X,L) do write(X), write(' ')),
	nl.

parse_record(Record, B1, B2, F) :-
	Record = [integer(B1,_), integer(B2,_), float(F,_)].

bin2chr(Bin, Species, Chr) :-
	chromosome(Species, Chr, A, B),
	Bin >= A,
	Bin =< B, !.

sort2(X, Y, X, Y) :- X =< Y, !.
sort2(X, Y, Y, X).

