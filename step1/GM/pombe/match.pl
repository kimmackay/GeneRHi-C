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

genome_size(pombe, 1258).
genome_size(elegans, 2008).

scale_factor(pombe, 1.0E9).
scale_factor(elegans, 1.0E4).

compile_adjacency(Path, Species) :-
	retractall(adjacency(_,_,_,_,_)),
	see(Path),
	read_record(_),
	repeat,
	  read_record(Record),
	  (   Record = end_of_file -> true
	  ;   parse_record(Record, B1, B2, F),
	      bin2chr(B1, Species, Chr1),
	      bin2chr(B2, Species, Chr2),
	      assertz(adjacency(B1, B2, F, Chr1, Chr2)),
	      fail
	  ), !,
	seen,
	save_predicates([adjacency/5], Species).


match_blossom5(Species, Set1, Set2) :-
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
	findall(edge(I,J,W), edge(I,J,W), Edges),
	length(Edges, E),
	genome_size(Species, N),
	scale_factor(Species, Scale),
	tell('/tmp/all.in'),
	NN is 2*N,
	EEN is 2*E+N,
	print_tab([NN,EEN]),
	(   foreach(edge(I1,J1,W1),Edges),
	    param(Scale)
	do  I11 is I1-1,
	    J11 is J1-1,
	    W11 is integer(-Scale*W1),
	    print_tab([I11,J11,W11])
	),
	(   foreach(edge(I2,J2,W2),Edges),
	    param(N,Scale)
	do  I21 is I2+N-1,
	    J21 is J2+N-1,
	    W21 is integer(-Scale*W2),
	    print_tab([I21,J21,W21])
	),
	(   for(K,1,N),
	    param(N)
	do  K1 is K-1,
	    K2 is K+N-1,
	    print_tab([K1,K2,0])
	),
	told,
	system('blossom5 -e /tmp/all.in -w /tmp/all.out'),
	see('/tmp/all.out'),
	retractall(solution(_,_,_)),
	read_line(_),
	repeat,
	  read_line(Codes),
	  (   Codes = end_of_file -> true
	  ;   append(Icodes, [0' |Jcodes], Codes),
	      number_codes(I0, Icodes),
	      number_codes(J0, Jcodes),
	      I is I0+1,
	      J is J0+1,
	      edge(I, J, W),
	      assertz(solution(I,J,W)),
	      fail
	  ), !,
	seen,
	findall(W, solution(_,_,W), Ws),
	length(Ws, Size),
	sumlist(Ws, Weight),
	format('% Blossom V computed a matching of size ~d and weight ~w\n', [Size,Weight]),
	findall(0, (solution(I,J,W), print_tab([I,J,W])), _).

print_tab(L) :-
	(foreach(X,L) do write(X), write(' ')),
	nl.


parse_record(Record, B1, B2, F) :-
	Record = [integer(B1,_), integer(B2,_), float(F,_)].

bin2chr(Bin, Species, Chr) :-
	chromosome(Species, Chr, A, B),
	Bin >= A,
	Bin =< B, !.
