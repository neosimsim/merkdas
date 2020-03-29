.POSIX:
ALL=build pdf
include ../include.mk

build:
	cabal build

pdf: hello-lenses.pdf

hello-lenses.pdf: bibliography.bib

run: build
	cabal run

clean: tex-clean
	cabal clean
