.PHONY: all

all: updown

updown: updown.scm
	csc updown.scm -o updown

clean:
	rm updown.scm
