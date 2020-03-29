.POSIX:
.SUFFIXES: .tex .pdf

AGDA=agda
TEX=xelatex

all: tutorial tutorial.pdf

tutorial:
	$(AGDA) -c --no-main Tutorial.lagda.tex

.tex.pdf:
	$(TEX) -interaction errorstopmode -jobname $* $<

tutorial.pdf: tutorial.tex Tutorial.lagda.tex

clean:
	rm -f *.agdai *.out *.pdf *.aux *.log
	rm -rf MAlonzo