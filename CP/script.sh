#!/bin/bash -x

# function run {
#     /opt/MiniZincIDE-2.1.5-bundle-linux-x86_64/mzn-gecode -a -s --fzn-flag -restart --fzn-flag luby --fzn-flag -nogoods --fzn-flag true --fzn-flag -p --fzn-flag 4 --fzn-flag -time --fzn-flag 600000 -D Chr1=$1 -D Chr2=$2 match3.mzn pombe.dzn
#     }
# function run {
#     mzn-chuffed -a -s --fzn-flag --time-out --fzn-flag 600 -D Chr1=$1 -D Chr2=$2 match3.mzn pombe.dzn
#     }
# function run {
#     mzn-or-tools -a -s --fzn-flag --fz_logging --fzn-flag --free_search --fzn-flag --time_limit --fzn-flag 600000 -D Chr1=$1 -D Chr2=$2 match3.mzn pombe.dzn
#     }
function run {
    mzn-or-tools -a -s --fzn-flag --free_search -D Chr1=$1 -D Chr2=$2 match3.mzn pombe.dzn
    }



CHR1=1..561
CHR2=562..1008
CHR3=1009..1258

# run $CHR1 $CHR1
# run $CHR2 $CHR2
run $CHR3 $CHR3
# run $CHR1 $CHR2
# run $CHR1 $CHR3
# run $CHR2 $CHR3
