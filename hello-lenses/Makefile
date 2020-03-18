.SUFFIXES: .lhs .pdf

TEX=xelatex
TEXFLAGS=-output-directory _build

all: build pdf

build:
	cabal build

pdf: hello-lenses.pdf

hello-lenses.pdf: bibliography.bib

run: build
	cabal run

.lhs.pdf:
	mkdir -p _build
	env TEXINPUTS=_build:.:texfiles: $(TEX) $(TEXFLAGS) -jobname $* $<
	if grep -q 'Please (re)run Biber on the file:' _build/$*.log; then\
		biber --input-directory _build --output-directory _build $*;\
		env TEXINPUTS=_build:.:texfiles: $(TEX) $(TEXFLAGS) $<;\
	fi
	if grep -q 'LaTeX Warning: There were undefined references.' _build/$*.log; then\
		env TEXINPUTS=_build:.:texfiles: $(TEX) $(TEXFLAGS) $<;\
	fi

clean: phony
	cabal clean
	rm -rf _build $(TEXOBJ)

phony:

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
