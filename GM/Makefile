SICSTUS = /home/matsc/sicstus4/bin/sicstus -f --nologo --noinfo

all: pombe_results/pombe_whole_match \
     pombe_results/pombe_cis1_match \
     pombe_results/pombe_cis2_match \
     pombe_results/pombe_cis3_match \
     pombe_results/pombe_trans1_match \
     pombe_results/pombe_trans2_match \
     pombe_results/pombe_trans3_match \
     elegans_results/elegans_whole_match \
     elegans_results/elegans_cis1_match \
     elegans_results/elegans_cis2_match \
     elegans_results/elegans_cis3_match \
     elegans_results/elegans_cis4_match \
     elegans_results/elegans_cis5_match \
     elegans_results/elegans_cis6_match \
     elegans_results/elegans_trans1_match \
     elegans_results/elegans_trans2_match \
     elegans_results/elegans_trans3_match \
     elegans_results/elegans_trans4_match \
     elegans_results/elegans_trans5_match \
     elegans_results/elegans_trans6_match \


pombe_results/pombe_whole_match: pombe.po
	echo "match_blossom5(pombe,[1,2,3],[1,2,3]), halt." | $(SICSTUS) -l match > $@

pombe_results/pombe_cis1_match: pombe.po
	echo "match_blossom5(pombe,[1],[1]), halt." | $(SICSTUS) -l match > $@

pombe_results/pombe_cis2_match: pombe.po
	echo "match_blossom5(pombe,[2],[2]), halt." | $(SICSTUS) -l match > $@

pombe_results/pombe_cis3_match: pombe.po
	echo "match_blossom5(pombe,[3],[3]), halt." | $(SICSTUS) -l match > $@

pombe_results/pombe_trans1_match: pombe.po
	echo "match_blossom5(pombe,[1],[2,3]), halt." | $(SICSTUS) -l match > $@

pombe_results/pombe_trans2_match: pombe.po
	echo "match_blossom5(pombe,[2],[1,3]), halt." | $(SICSTUS) -l match > $@

pombe_results/pombe_trans3_match: pombe.po
	echo "match_blossom5(pombe,[3],[1,2]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_whole_match: elegans.po
	echo "match_blossom5(elegans,[1,2,3,4,5,6],[1,2,3,4,5,6]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_cis1_match: elegans.po
	echo "match_blossom5(elegans,[1],[1]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_cis2_match: elegans.po
	echo "match_blossom5(elegans,[2],[2]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_cis3_match: elegans.po
	echo "match_blossom5(elegans,[3],[3]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_cis4_match: elegans.po
	echo "match_blossom5(elegans,[4],[4]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_cis5_match: elegans.po
	echo "match_blossom5(elegans,[5],[5]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_cis6_match: elegans.po
	echo "match_blossom5(elegans,[6],[6]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_trans1_match: elegans.po
	echo "match_blossom5(elegans,[1],[2,3,4,5,6]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_trans2_match: elegans.po
	echo "match_blossom5(elegans,[2],[1,3,4,5,6]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_trans3_match: elegans.po
	echo "match_blossom5(elegans,[3],[1,2,4,5,6]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_trans4_match: elegans.po
	echo "match_blossom5(elegans,[4],[1,2,3,5,6]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_trans5_match: elegans.po
	echo "match_blossom5(elegans,[5],[1,2,3,4,6]), halt." | $(SICSTUS) -l match > $@

elegans_results/elegans_trans6_match: elegans.po
	echo "match_blossom5(elegans,[6],[1,2,3,4,5]), halt." | $(SICSTUS) -l match > $@

pombe.po:	match.pl
	echo "compile_adjacency('../../data/s_pombe/adjacency_matrices/floating_point_values/GSM1379427_adjacency_matrix.csv', pombe), halt." | $(SICSTUS) -l match

elegans.po:	match.pl
	echo "compile_adjacency('../../data/c_elegans/adjacency_matrices/floating_point_values/N2-DpnII_adjacency_matrix.csv', elegans), halt." | $(SICSTUS) -l match

