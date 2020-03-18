.POSIX:
.SUFFIXES: .tex .pdf

TEX=xelatex
TEXFLAGS=--interaction errorstop-mode

all: logo.pdf

.tex.pdf:
	$(TEX) $(TEXFLAGS) $*
	if grep -q 'Please (re)run Biber on the file:' $*.log; then biber $*; $(TEX) $(TEXFLAGS) $*; fi
	if grep -q 'LaTeX Warning: There were undefined references.' $*.log; then $(TEX) $(TEXFLAGS) $*; fi

TEXOBJ = \
	*.aux \
	*.bbl \
	*.bcf \
	*.blg \
	*.log \
	*.out \
	*.pdf \
	*.run.xml \
	*.toc \

clean: phony
	rm -f $(TEXOBJ)

phony:

