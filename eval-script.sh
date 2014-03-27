
if [ $# -ne 4 ]
    then
        echo "Wrong number of arguments."
        echo "script <scaffold_file> <contig_file> <path_to_reference_genome> <path_to_gapseq>"
        exit -1
fi

perl /home/gisv85/script/gapFill/selectContigAndMap.pl $1 $2 $3

perl /home/gisv85/script/gapFill/checkGapUsingDis.pl newContig-ref.scaf \
$1 newContigGapStatus

perl /home/gisv85/script/gapFill/checkGapAlignmentVelvet.pl \
$3 $1 $4 newContigGapStatus filledGapStatistic