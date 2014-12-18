TARGETS = test

# We need to include the external libraries we used
ASDF_TREE = --asdf-tree ~/quicklisp/dists/quicklisp/software/

all: $(TARGETS)

test:
	sbcl --load test.lisp --non-interactive --noprint

.PHONY:	all test
