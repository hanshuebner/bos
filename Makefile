all: bos.core
.PHONY: all

bos.core: build.lisp
	env LANG=en_US.UTF-8 sbcl --load build.lisp --eval '(sb-ext:save-lisp-and-die "bos.core")'

# various cleaning stuff
.PHONY: cleancore
cleancore:
	rm -f bos.core

.PHONY: cleanfasl
cleanfasl:
	(cd ../.. && sbcl --disable-debugger --load clean.lisp --eval '(quit)')

.PHONY: cleanall
cleanall: cleancore cleanfasl

.PHONY: clean
clean: cleancore

.PHONY: start
start: bos.core
	sbcl --core bos.core --eval '(start)'

# TAGS
TAGS:
	find . -name '*.lisp' | xargs etags -a
