## script for calculating xyz coordinates in GeneRHi-C
## coordinates will be output in a csv format (Node,X,Y,Z)
## Argument 1: the input file name ( .gexf file)
## Argument 2: the output file name
##
## Kimberly MacKay Oct 16, 2019
## license: This work is licensed under the Creative Commons Attribution-NonCommercial-
## ShareAlike 3.0 Unported License. To view a copy of this license, visit 
## http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 
## PO Box 1866, Mountain View, CA 94042, USA.

import sys
import csv

## GENERATE THE GRPAPH
infilename = sys.argv[1]
infile = open(infilename, 'r')

allNodes = []
nodeID = ""
xyz = []

for line in infile:
	# if we are at a new node
	if "<node id=" in line:
		# add the old information to the list
		allNodes.append([nodeID] + xyz)
			
		l = line.split('"')
			
		# resent the variables
		nodeID = l[3]
		xyz = []
	# if we are at a line with position information
	elif "<viz:position" in line:
		# split the line
		l = line.split('"')
			
		# store the position information
		xyz = [l[1], l[3]]

# add the remaining node information	
allNodes.append([nodeID] + xyz)

# output the results	
outfilename = sys.argv[2]
header = ["Node", "X", "Y", "Z"]

outfile = open(outfilename, 'w')
csv_wr = csv.writer(outfile)

csv_wr.writerow(header)
csv_wr.writerows(allNodes)

infile.close()
outfile.close()