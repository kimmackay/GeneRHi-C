Data was downloaded from: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE56849

Download date: 2017/10/02

Links to specific samples:

	- GSM1379427: 999a, wild-type (https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM1379427)
	- GSM1379429: G1-arrested (https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM1379429)
	- GSM1379430: rad21-K1, mutant (https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM1379430)
	- GSM1379431: Clr4D, mutant (https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM1379431)

Original Paper: Mizuguchi T, Fudenberg G, Mehta S, Belton JM et al. Cohesin-dependent globules and heterochromatin shape 3D genome architecture in S. pombe. Nature 2014 Dec 18;516(7531):432-435. PMID: 25307058

Link to Original paper: http://www.nature.com/nature/journal/v516/n7531/abs/nature13833.html

Data was preprocessed and normalized using the library:  https://bitbucket.org/mirnylab

the resultant normalized whole-genome contact maps are located in ./normalized_matrices 

these matrices were used as input to ../scripts/generate_adjacency_matrix.pl to generate the non-redundant, CSV, adjacency matrices (located in adjacency_matrices)

bin_indices.txt contains the linear bin indices for each chromosomes
