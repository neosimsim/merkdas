.POSIX:
ALL=tutorial tutorial.pdf
include ../include.mk

tutorial:
	$(AGDA) -c --no-main Tutorial.lagda.tex

tutorial.pdf: tutorial.tex Tutorial.lagda.tex

clean: tex-clean
	rm -rf MAlonzo
