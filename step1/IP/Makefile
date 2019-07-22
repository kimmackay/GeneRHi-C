SICSTUS = /home/matsc/sicstus4/bin/sicstus -f --nologo --noinfo

all: pombe_cis1_m5 \
     pombe_cis2_m5 \
     pombe_cis3_m5 \
     pombe_trans1_m5 \
     pombe_trans2_m5 \
     pombe_trans3_m5 \
     pombe_cis1_m1 \
     pombe_cis2_m1 \
     pombe_cis3_m1 \
     pombe_trans1_m1 \
     pombe_trans2_m1 \
     pombe_trans3_m1 \
     pombe_whole_m1 \
     pombe_whole_m2 \
     pombe_whole_m3 \
     pombe_whole_m4 \
     pombe_whole_m5 \
     pombe_whole_m6 \


pombe_whole_m1: pombe.po ip.pl
	echo "solve_ip(pombe,[1,2,3],[1,2,3],1), halt." | $(SICSTUS) -l ip.pl > $@

pombe_whole_m2: pombe.po ip.pl
	echo "solve_ip(pombe,[1,2,3],[1,2,3],2), halt." | $(SICSTUS) -l ip.pl > $@

pombe_whole_m3: pombe.po ip.pl
	echo "solve_ip(pombe,[1,2,3],[1,2,3],3), halt." | $(SICSTUS) -l ip.pl > $@

pombe_whole_m4: pombe.po ip.pl
	echo "solve_ip(pombe,[1,2,3],[1,2,3],4), halt." | $(SICSTUS) -l ip.pl > $@

pombe_whole_m5: pombe.po ip.pl
	echo "solve_ip(pombe,[1,2,3],[1,2,3],5), halt." | $(SICSTUS) -l ip.pl > $@

pombe_whole_m6: pombe.po ip.pl
	echo "solve_ip(pombe,[1,2,3],[1,2,3],6), halt." | $(SICSTUS) -l ip.pl > $@

pombe_cis1_m5: pombe.po ip.pl
	echo "solve_ip(pombe,[1],[1],5), halt." | $(SICSTUS) -l ip.pl > $@

pombe_cis2_m5: pombe.po ip.pl
	echo "solve_ip(pombe,[2],[2],5), halt." | $(SICSTUS) -l ip.pl > $@

pombe_cis3_m5: pombe.po ip.pl
	echo "solve_ip(pombe,[3],[3],5), halt." | $(SICSTUS) -l ip.pl > $@

pombe_trans1_m5: pombe.po ip.pl
	echo "solve_ip(pombe,[1],[2,3],5), halt." | $(SICSTUS) -l ip.pl > $@

pombe_trans2_m5: pombe.po ip.pl
	echo "solve_ip(pombe,[2],[1,3],5), halt." | $(SICSTUS) -l ip.pl > $@

pombe_trans3_m5: pombe.po ip.pl
	echo "solve_ip(pombe,[3],[1,2],5), halt." | $(SICSTUS) -l ip.pl > $@

pombe_cis1_m1: pombe.po ip.pl
	echo "solve_ip(pombe,[1],[1],1), halt." | $(SICSTUS) -l ip.pl > $@

pombe_cis2_m1: pombe.po ip.pl
	echo "solve_ip(pombe,[2],[2],1), halt." | $(SICSTUS) -l ip.pl > $@

pombe_cis3_m1: pombe.po ip.pl
	echo "solve_ip(pombe,[3],[3],1), halt." | $(SICSTUS) -l ip.pl > $@

pombe_trans1_m1: pombe.po ip.pl
	echo "solve_ip(pombe,[1],[2,3],1), halt." | $(SICSTUS) -l ip.pl > $@

pombe_trans2_m1: pombe.po ip.pl
	echo "solve_ip(pombe,[2],[1,3],1), halt." | $(SICSTUS) -l ip.pl > $@

pombe_trans3_m1: pombe.po ip.pl
	echo "solve_ip(pombe,[3],[1,2],1), halt." | $(SICSTUS) -l ip.pl > $@



pombe.po:	../../GM/run_results/match.pl
	echo "compile_adjacency('../../data/s_pombe/adjacency_matrices/floating_point_values/GSM1379427_adjacency_matrix.csv', pombe), halt." | $(SICSTUS) -l ../../GM/run_results/match.pl

elegans.po:	../../GM/run_results/match.pl
	echo "compile_adjacency('../../data/c_elegans/adjacency_matrices/floating_point_values/N2-DpnII_adjacency_matrix.csv', elegans), halt." | $(SICSTUS) -l ../../GM/run_results/match.pl
