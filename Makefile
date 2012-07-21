
# This shows how to create a Makefile that can automatically
# convert .sscm (sweet-Scheme) into Scheme, using
# Make "pattern rules".
# Just put this into your "Makefile":

SWEET-FILTER = ./sweet-filter

%.scm : %.sscm
	$(SWEET-FILTER) < $< > $@


all: sweetener.scm


itest: sweetener.scm
	@echo "(a (b c) (d) ((e f) g (h)) i (k l m) (+ x y) 'j z)" \
                | sweetener

