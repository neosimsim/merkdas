.SUFFIXES: .pdf .tex .lhs

AGDA=agda
TEX=xelatex
TEXOBJ = \
	*.aux \
	*.bbl \
	*.bcf \
	*.blg \
	*.log \
	*.nav \
	*.out \
	*.pdf \
	*.run.xml \
	*.snm \
	*.toc \
	*.vrb \

.tex.pdf .lhs.pdf:
	$(TEX) $(TEXFLAGS) -jobname $* $<
	if grep -q 'Please (re)run Biber on the file:' $*.log; \
	then \
		biber $*; \
		$(TEX) $(TEXFLAGS) $<; \
	fi
	if grep -q 'LaTeX Warning: There were undefined references.' $*.log; then\
		env $(TEX) $(TEXFLAGS) $<;\
	fi

all: $(ALL)

phony:

tex-clean: phony
	rm -f $(TEXOBJ)

c-clean: phony
	rm -f *.o