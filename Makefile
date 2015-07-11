.PHONY: all

all: updown

updown: updown.scm
	csc -O3 updown.scm -o updown

clean:
	rm updown
