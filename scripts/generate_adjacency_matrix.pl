#!/usr/bin/perl
## will generate a non-redundant, CSV, adjacency matrix 
##
## argument 1: the normalized whole-genome contact map
## argument 2: a value to scale the interaction frequencies from the whole-genome contact map (enter 1 if you wish to not scale the values)
## argument 3: 1 if you wish to output integer, scaled interaction frequencies, 0 if you wish to output the original values in the whole-genome contact map
##
## Kimberly MacKay October 11, 2017
## license: This work is licensed under the Creative Commons Attribution-NonCommercial-
## ShareAlike 3.0 Unported License. To view a copy of this license, visit 
## http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 
## PO Box 1866, Mountain View, CA 94042, USA.

use strict;
use warnings;

## check to ensure three arguments were passed in
die "ERROR: must pass in three arguments." if @ARGV != 3;

## extract the command line arguments
my $hic_file = $ARGV[0];
my $scale = $ARGV[1];
my $integer_values = $ARGV[2];

## open the interaction matrix file
open WGCM, "$hic_file" or die "ERROR: $hic_file could not be opened.";
chomp(my @hic_matrix = <WGCM>);
close WGCM;

## convert the decimals to integers and store them in a new array
## note: the 0th row and column of freq will be empty allow for a more natural
## parsing later on 
my @frequencies;

## for each line after the header line
for(my $row = 1; $row <= $#hic_matrix; $row++)
{
	## split the line
	my @matrix_line = split /\t/, $hic_matrix[$row];
	
	## loop through the entire file to extract the frequencies
	for(my $col = 1; $col <= $#hic_matrix; $col++)
	{
		## adjusts NA's to 0's
		if($matrix_line[$col] =~ "NA")
		{
			$frequencies[$row][$col] = 0;
		}
		else
		{
			if($integer_values)
			{
				## convert the interaction frequencies into a scaled integer
				$frequencies[$row][$col] = int($matrix_line[$col]*$scale);
			}
			else
			{
				## otherwise leave them as is
				$frequencies[$row][$col] = $matrix_line[$col];
			}
		}
	}
}

## print the header line
print "bin_label_1,bin_label_2,interaction_frequency\n";

## loop through one half of the matrix and print out non-zero interactions
## in adjacency matrix, CSV format
## avoid the diagonal to prevent self-self interactions
for(my $row = 1; $row <= $#frequencies; $row++)
{
	for(my $col = $row+1; $col <= $#frequencies; $col++)
	{
		## if it is a non-zero frequency
		if($frequencies[$row][$col] != 0)
		{
			print "$row,$col,$frequencies[$row][$col]\n";
		}
	}
}
