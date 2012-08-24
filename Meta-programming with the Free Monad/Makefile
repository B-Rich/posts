USER=johnw
HOST=www\.newartisans\.com
PASS=$(shell grep '^machine $(HOST)' ~/.authinfo | awk '{print $$6}')

all: using-fix.html

%.html: %.lhs
	BlogLiterately --ghci $< > $@

upload:
	BlogLiterately --ghci --upload-images				\
	               --blog http://www.newartisans.com/xmlrpc.php	\
	               --user "$USER" --password "$PASS"		\
		       --postid 299					\
                       --title "Using the fix combinator"		\
		       --category "Haskell"				\
	               using-fix.lhs

### Makefile ends here
