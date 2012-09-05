# -*- Makefile -*-

USER=johnw
HOST=www.newartisans.com
PASS=$(shell grep '^machine $(HOST)' ~/.authinfo | awk '{print $$6}')

SOURCE=$(wildcard *.lhs)
DEST=$(patsubst %.lhs,%.html,$(SOURCE))

all: $(DEST)

clean:
	rm $(DEST)

%.html: %.lhs
	literate --hs-kate --ghci $< > $@

upload: $(SOURCE)
	literate --hs-kate --ghci --upload-images			\
	         --blog http://$(HOST)/xmlrpc.php			\
	         --user "$(USER)" --password "$(PASS)"			\
                 --title "$(shell head -1 info)"			\
		 --postid $(shell tail -1 info)				\
		 --category $(shell head -2 info | tail -1)		\
	         $<

### Makefile ends here
