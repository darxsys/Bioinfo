# script to produce the graphs for finis
Rscript finis-stats.r ../finis/celegans/r-stats-celegans.out \
    ../finis/celegans/graphs/celegans Celegans
sed -i 's/drosophila/celegans/g' graphs.tex
sed -i 's/Drosophila/Celegans/g' graphs.tex
pdflatex --output-directory=tex/ graphs.tex
mv tex/graphs.pdf tex/celegans-graphs.pdf 

Rscript finis-stats.r ../finis/drosophila/r-stats-drosophila.out \
    ../finis/drosophila/graphs/drosophila Drosophila
sed -i 's/celegans/drosophila/g' graphs.tex
sed -i 's/Celegans/Drosophila/g' graphs.tex
pdflatex --output-directory=tex/ graphs.tex
mv tex/graphs.pdf tex/drosophila-graphs.pdf 


