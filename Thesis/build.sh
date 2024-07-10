#!/bin/bash

RUN_R_COMMAND=false

while [[ "$#" -gt 0 ]]; do
    case $1 in
        -r|--run-r) RUN_R_COMMAND=true; shift ;;
        *) echo "Unknown parameter passed: $1"; exit 1 ;;
    esac
done

if [ "$RUN_R_COMMAND" = true ]; then
    R CMD Rd2pdf ../../PCMNMA
    mv PCNMA.pdf Chapters/
fi

xelatex main.tex
bibtex main
xelatex main.tex
xelatex main.tex
texcount -inc -html -v -sum main.tex > wordcount.html