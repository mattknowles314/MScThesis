import os

os.chdir("Thesis/")
os.system("./build.sh")

unclean_files = [".aux", ".log", ".fdb_latexmk", ".toc", ".fls", ".lof", ".blg", ".bbl"]

for i in unclean_files:
    os.system("rm *"+i)

print("Wordcount: \n")
os.system("detex main.tex | wc -w | tr -d '[:space:]'")

os.chdir("../")
