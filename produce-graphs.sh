#!/bin/bash
# script to produce the graphs for finis
# scripts takes in the following:
# path to the r script, 
# path to the r stats file
# path to the latex generating script
# r output folder
# path to the latex output folder
# data name
# data prefix
# script will create folders if they don't exist
# output pdf name

if [ $# -ne 8 ]
    then
        echo "Wrong number of arguments."
        echo -n "script <rscript> <r-stats-file> <latex-script>"
        echo -n " <r-output-folder> <latex-output-folder> <data-name>"
        echo " <out-pdf-name>"
        exit -1
fi

rscript=$1
rstats=$2
tex_script=$3
r_out=$4
latex_out=$5
data_name=$6
data_prefix=$7
pdf_name=$8

mkdir -p $r_out
mkdir -p $latex_out

Rscript $rscript $rstats "$r_out" $data_name

sed -i "s/drosophila/$data_prefix/g" $tex_script
sed -i "s/Drosophila/$data_name/g" $tex_script
sed -i "s/celegans/$data_prefix/g" $tex_script
sed -i "s/Celegans/$data_prefix/g" $tex_script
cp $tex_script $r_out

# pdflatex --output-directory=$latex_out 
# mv "$latex_out/graphs.pdf" "$latex_out/""$pdf_name"
