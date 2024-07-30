#/bin/bash

start=$(date +%s)
xelatex -no-pdf main.tex 
bibtex main
xelatex -no-pdf main.tex
xelatex main.tex
texcount -inc -html -v -sum main.tex > wordcount.html
end=$(date +%s)
runtime=$((end-start))
echo "compiletime: $runtime s"

