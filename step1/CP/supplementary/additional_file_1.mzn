%% Load the relevant libraries
include "globals.mzn";

%% Variable Declarations
int: N;

%% first chromosome of interest
set of 1..N: Chr1; 

%% second chromosome of interest
set of 1..N: Chr2; 

%% the given frequency map, assumed symmetric, main diagonal = 0
array[1..N,1..N] of int: map; 

%% main decision variables
array[1..N] of var 1..N: match; 

%% objective per row
array[1..N] of var int: rowobj; 

%% total objective, N.B. counting each binding twice
var int: dobj; 

%% mask out all entries not connecting Chr1 and Chr2
int: masked_map(1..N: i, 1..N: j) =
  if i in Chr1 /\ j in Chr2 then
    map[i,j]
  else if i in Chr2 /\ j in Chr1 then
    map[i,j]
  else 0 endif endif;

%% assertion: frequency map is symmetric
constraint
  forall(i in 1..N, j in 1..N where i<j)
  	(assert(masked_map(i,j) = masked_map(j,i), "Asymmetry!"));

%% constrain the objective, one slice per row
constraint
  forall(i in 1..N)
  	(rowobj[i] = [masked_map(i,j) | j in 1..N][match[i]]);

%% constrain the total objective
constraint
  dobj = sum(i in 1..N)(rowobj[i]);

%% domination: prevent zero-frequency edges
constraint
  forall(i in 1..N)
  	(match[i] in {i} union {j | j in 1..N where masked_map(i,j)>0});

%% domination: prevent obviously suboptimal solutions
constraint
  forall(i in 1..N, j in 1..N where
  	masked_map(i,j)>0)(rowobj[i] + rowobj[j] >= 1);

%% essential matching constraint
constraint
  inverse(match, match) :: domain;

%% Solve
solve :: int_search(rowobj, max_regret, indomain_max, complete)
  maximize(dobj);

%% output the results
output
 ["edge(\(i),\(match[i])). % benefit = \(rowobj[i])\n" |
 	 i in 1..N where fix(match[i])>i] ++
 ["objective(\(dobj div 2)).\n"] ++
 [];