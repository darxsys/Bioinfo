#!/bin/bash
# script to produce the graphs for finis
# scripts takes in the following:
# path to the r script, 
# path to the r stats file
# name of the latex generating script
# r output folder
# data name
# script will create folders if they don't exist
# output pdf name

if [ $# -ne 6 ]
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
data_name=$5
pdf_name=$6
latex_out=$r_out"/tex"

mkdir -p $r_out
mkdir -p $latex_out

echo "Running R script..."
Rscript $rscript $rstats "$r_out" $data_name > /dev/null
echo "Statistics generated."

cp $tex_script $r_out
tex_path="$r_out/$tex_script"

current_dir=$(pwd)
cd $r_out

echo "Running latex..."
pdflatex --output-directory="./tex" $tex_script  #> /dev/null
mv "./tex/graphs.pdf" "./$pdf_name"
echo "Pdf with graphs generated."

echo "Removing files..."
rm -rf "./tex"
cd $current_dir
rm -rf $tex_path
echo "Done."
