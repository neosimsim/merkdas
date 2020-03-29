.POSIX:
.SUFFIXES: .tex .pdf

AGDA=agda
TEX=xelatex

build:
	$(AGDA) --ghc-dont-call-ghc part1/Naturals.lagda.tex

pdf: plfa.pdf

html: phony
	make4ht -d html -e plfa.mk4 -f html+mathjaxnode -nux plfa.tex

plfa.pdf: part1/Naturals.lagda.tex

.tex.pdf:
	$(TEX) $(TEXFLAGS) $*
	if grep -q 'Please (re)run Biber on the file:' $*.log; then biber $*; $(TEX) $(TEXFLAGS) $*; fi
	if grep -q 'LaTeX Warning: There were undefined references.' $*.log; then $(TEX) $(TEXFLAGS) $*; fi

phony: