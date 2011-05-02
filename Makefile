all: bos.image
.PHONY: all

SBCL_BUILD=env LC_CTYPE=en_US.UTF-8 HOME=/home/bknr sbcl --dynamic-space-size 800 --no-userinit
SBCL_RUN=env LC_CTYPE=en_US.ISO8859-1 sbcl --core bos.core --dynamic-space-size 800 --no-userinit

CCL_BUILD=ccl -n
CCL_RUN=ccl -n

all: bos.image
start: start-ccl

bos.core: build.lisp
	$(SBCL_BUILD) --load build.lisp --eval '(sb-ext:save-lisp-and-die "bos.core")'

bos.image: build.lisp
	$(CCL_BUILD) -l build.lisp -e '(ccl:save-application "bos.image")'

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

.PHONY: start-sbcl start-ccl
start-sbcl: bos.core
	$(SBCL_RUN) --eval '(start)'

start-ccl: bos.image
	$(CCL_RUN) -e '(start)'

# TAGS
TAGS:
	find . -name '*.lisp' | xargs etags -a
