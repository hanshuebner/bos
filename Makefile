all: bos.core
.PHONY: all

SBCL_BUILD=env LC_CTYPE=en_US.UTF-8 HOME=/home/bknr sbcl --dynamic-space-size 800 --no-userinit
SBCL_RUN=env LC_CTYPE=en_US.ISO8859-1 sbcl --core bos.core --dynamic-space-size 800 --no-userinit

bos.core: build.lisp
	$(SBCL_BUILD) --load build.lisp --eval '(sb-ext:save-lisp-and-die "bos.core")'

# various cleaning stuff
.PHONY: cleancore
cleancore:
	rm -f bos.core

.PHONY: cleanfasl
cleanfasl:
	(cd ../.. && sbcl --no-userinit --disable-debugger --load clean.lisp --eval '(quit)')

.PHONY: cleanall
cleanall: cleancore cleanfasl

.PHONY: clean
clean: cleancore

.PHONY: start
start: bos.core
	$(SBCL_RUN) --eval '(start)'

.PHONY: start-cert-daemon
start-cert-daemon: bos.core
	$(SBCL_RUN) --eval '(bos.m2.cert-generator:cert-daemon)'

# TAGS
TAGS:
	find . -name '*.lisp' | xargs etags -a
