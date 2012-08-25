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
	cat $<						\
	    | perl -ne 'print unless /-- HIDE/'		\
	    | perl -pe 's/^\\#/#/;'			\
	    > /tmp/$<
	literate --ghci /tmp/$< > $@
	@rm -f /tmp/$<

upload: $(SOURCE)
	cat $<						\
	    | perl -ne 'print unless /-- HIDE/'		\
	    | perl -pe 's/^\\#/#/;'			\
	    > /tmp/$<
	literate --ghci --upload-images					\
	         --blog http://$(HOST)/xmlrpc.php			\
	         --user "$(USER)" --password "$(PASS)"			\
                 --title "$(shell head -1 info)"			\
		 --postid $(shell tail -1 info)				\
		 --category $(shell head -2 info | tail -1)		\
	         /tmp/$<
	@rm -f /tmp/$<

publish: $(SOURCE)
	cat $<						\
	    | perl -ne 'print unless /-- HIDE/'		\
	    | perl -pe 's/^\\#/#/;'			\
	    > /tmp/$<
	literate --ghci --upload-images --publish			\
	         --blog http://$(HOST)/xmlrpc.php			\
	         --user "$(USER)" --password "$(PASS)"			\
                 --title "$(shell head -1 info)"			\
		 --postid $(shell tail -1 info)				\
		 --category $(shell head -2 info | tail -1)		\
	         /tmp/$<
	@rm -f /tmp/$<

### Makefile ends here
