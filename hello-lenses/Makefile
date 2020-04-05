.POSIX:
ALL=build pdf
include ../include.mk

build:
	cabal build

pdf: hello-lenses.pdf

hello-lenses.pdf: bibliography.bib Plain.lhs Records.lhs Lenses.lhs

run: build
	cabal run

clean: tex-clean
	cabal clean
