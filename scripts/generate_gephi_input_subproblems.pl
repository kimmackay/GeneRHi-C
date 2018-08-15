#!/usr/bin/perl
## generates the two files (nodes.tsv and edges.tsv) needed for gephi visualization
##
## argument 1: the number of chromsomes
## argument 2: the experimental resolution
## argument 3: the number of subproblems (or result files that need to be integrated)
## agrument 4: the beginning out the output files names
##
## Kimberly MacKay Nov 9, 2017
## license: This work is licensed under the Creative Commons Attribution-NonCommercial-
## ShareAlike 3.0 Unported License. To view a copy of this license, visit 
## http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 
## PO Box 1866, Mountain View, CA 94042, USA.

use strict;
use warnings;

## check to ensure four arguments were passed in
die "ERROR: must pass in four arguments." if @ARGV != 4;

my $num_chr = $ARGV[0];
my $experimental_resolution = $ARGV[1];
my $num_sub_problems = $ARGV[2];
my $out_file = $ARGV[3];

# scan each of the results files and collect the result in this hash table
my %interactions;
my %dynamics_coefficents;

###########################################################################################
##	initialize variables
###########################################################################################
## get the ending index of each chromosome
my @stop_index;
for(my $chr = 1; $chr <= $num_chr; $chr++)
{
	print STDERR "What is the ending index of CHR".$chr."? ";
	my $input = <STDIN>;
	$stop_index[$chr] = int($input);
}

$stop_index[0] = 0;

## make an array that maps genomic bin to the corresponding chromosome
my @chrs;
my $bin = 1;
for(my $j = 1; $j <= $num_chr; $j++)
{
	for(my $i = $bin; $i <= $stop_index[$j]; $i++)
	{
			$chrs[$bin] = $j;
			$bin = $bin +1;
	}
}


###########################################################################################
##	print the *_nodes.tsv file
###########################################################################################

## open the *_nodes.tsv file for printing
open(my $NODE_FILE, '>', $out_file."_nodes.tsv") or die "Could not open file: ".$out_file."_nodes.tsv";

# print the header line
print $NODE_FILE "id \t label \t chromosome\n";

# chromosome counter
my $c = 1;

# for each genomic bin;
for(my $row = 1; $row <= $stop_index[$num_chr]; $row++)
{	
	# print the node information 
	print $NODE_FILE $row."\tbin".$row."\t$c\n";

	if($row == $stop_index[$c])
	{
		# increment the chromosome counter
		$c = $c + 1;
	}
}

# close the *_nodes.tsv file
close $NODE_FILE;


###########################################################################################
##	print the *_edges.tsv file
###########################################################################################

## NOTE: when using the ForceAtlas2 layout - a larger weight will result in nodes being closer together. In this layout, edge weight represents the strength of the attraction
## open the *_edges.tsv file for printing
open(my $EDGE_FILE, '>', $out_file."_edges.tsv") or die "Could not open file: ".$out_file."_edges.tsv";

# print the header line
print $EDGE_FILE "Source\tTarget\tType_of_interaction\tWeight\n";

# chromosome counter
$c = 1;

# print out the linear interactions and their "distances" according to the 
# frequency value or experimental resolution (for s.pombe was 10 kb)
for(my $row = 1; $row <= $stop_index[$num_chr]; $row++)
{	
	if($row < $stop_index[$c])
	{
		# experimental resolution is not inverted since edge weight represents the strength of the attraction
		print $EDGE_FILE $row."\t".($row+1)."\tlinear".$c."\t".($experimental_resolution)."\n";
	}
	else
	{
		# increment the chromosome counter
		$c = $c + 1;
	}
	
	# initialize the %dynamics_coefficients hash for each bin to be = 0
	$dynamics_coefficents{$row} = 0;
}

##	Scan the result files

# for each intra-interaction
for(my $sp = 0; $sp < $num_sub_problems; $sp++)
{
	# read in the result file
	print STDERR "subproblem ".($sp+1).": give the path for the results file: ";
	chomp(my $out_file = <STDIN>);
	
	## open the file
	open RESULTS, "$out_file" or die "ERROR: $out_file could not be opened.";
	chomp(my @results_file = <RESULTS>);
	close RESULTS;

	# a boolean flag to let us know once the adjacency results start
	my $read_results = 0;

	for(my $i = 0; $i <= $#results_file; $i++)
	{	
	
	if($read_results)
	{
		## split the line from the clp file
		my @results_line =  split /\s+/, $results_file[$i];
	
		## get node1
		my $node1 = $results_line[0];
	
		## get node(s)2
		my $node2 = $results_line[1];
	
		## get the corresponding frequency - scale by $experimental_resolution
		my $freq = $results_line[2] * $experimental_resolution;
	
		## determine if it is a cis- or trans- interaction
		my $chr1 = $chrs[$results_line[0]];
		my $chr2 = $chrs[$results_line[1]];
	
		## if it is a cis- interaction
		if($chr1 == $chr2)
		{
			# add it to the hash
			$interactions{"cis_".$node1."_".$node2}{NODE1_BIN} = $node1;
#			$interactions{"cis_".$node1."_".$node2}{NODE1_CHR} = $chr1;
					
			$interactions{"cis_".$node1."_".$node2}{NODE2_BIN} = $node2;
#			$interactions{"cis_".$node1."_".$node2}{NODE2_CHR} = $chr2;
					
			$interactions{"cis_".$node1."_".$node2}{INTERACTION_FREQ} = $freq;
			$interactions{"cis_".$node1."_".$node2}{INTERACTION_TYPE} = "cis";
			
			$dynamics_coefficents{$node1} = $dynamics_coefficents{$node1} + 1;
			$dynamics_coefficents{$node2} = $dynamics_coefficents{$node2} + 1;
		}
		## if it is a trans- interaction 
		else
		{
			# add it to the hash
			$interactions{"trans_".$node1."_".$node2}{NODE1_BIN} = $node1;
			$interactions{"trans_".$node1."_".$node2}{NODE1_CHR} = $chr1;
					
			$interactions{"trans_".$node1."_".$node2}{NODE2_BIN} = $node2;
			$interactions{"trans_".$node1."_".$node2}{NODE2_CHR} = $chr2;
					
			$interactions{"trans_".$node1."_".$node2}{INTERACTION_FREQ} = $freq;
			$interactions{"trans_".$node1."_".$node2}{INTERACTION_TYPE} = "trans";
			
			$dynamics_coefficents{$node1} = $dynamics_coefficents{$node1} + 1;
			$dynamics_coefficents{$node2} = $dynamics_coefficents{$node2} + 1;
		}
	}
	
	## check to see if we should start reading during the next iteration; results start after a comment line beginning with %
	if($results_file[$i] =~ /^%/)
	{
		$read_results = 1;
	} 
	
	}								
}

###########################################################################################
## print out the cis and trans interactions and their "distances" according to the 
## frequency value and dynamics coefficient
###########################################################################################

foreach my $selected_interaction (sort keys %interactions) 
{
		# calculate the average dynamics coefficient between the two bins involved in the interactions
		my $d_coefficient = ($dynamics_coefficents{$interactions{$selected_interaction}{NODE1_BIN}} + $dynamics_coefficents{$interactions{$selected_interaction}{NODE2_BIN}})/2;
		
		# print the interaction	
		print $EDGE_FILE $interactions{$selected_interaction}{NODE1_BIN}."\t".$interactions{$selected_interaction}{NODE2_BIN}."\t".$interactions{$selected_interaction}{INTERACTION_TYPE}."\t".($interactions{$selected_interaction}{INTERACTION_FREQ}/$d_coefficient)."\n";
}	

END {
  warn "this script took ", time - $^T, " seconds\n";
}