.POSIX:
ALL=build pdf

include ../include.mk

build:
	$(AGDA) --ghc-dont-call-ghc part1/Naturals.lagda.tex

pdf: plfa.pdf

plfa.pdf: part1/Naturals.lagda.tex references.bib

clean: tex-clean