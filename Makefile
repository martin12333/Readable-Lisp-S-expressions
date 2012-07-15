
# This shows how to create a Makefile that can automatically
# convert .sscm (sweet-Scheme) into Scheme, using
# Make "pattern rules".
# Just put this into your "Makefile":

SWEET-FILTER = ./sweet-filter

%.scm : %.sscm
	$(SWEET-FILTER) < $< > $@


all: iformat.scm
