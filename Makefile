all: bos.core
.PHONY: all

cmucl.core:
	lisp -load make-base-lisp.lisp

bos.core: cmucl.core build.sh load.lisp build.lisp
	./build.sh

# run with slime
.PHONY: slime
slime: bos.core
	lisp -core bos.core -slime

# test

.PHONY: test
test: bos.core
	lisp -core bos.core -run-tests -slime

# various cleaning stuff
.PHONY: cleancore
cleancore:
	rm -f cmucl.core
	rm -f bos.core

.PHONY: cleanfasl
cleanfasl:
	(cd ../.. && find . -name '*.x86f' | xargs rm)

.PHONY: cleanall
cleanall: cleancore cleanfasl

.PHONY: clean
clean: cleancore

# I once had the problem that compiling
# and loading a lisp source was fine,
# but loading the pre-compiled fasl failed...

.PHONY: crazy_build_test
crazy_build_test:
	make cleanall
	make all
	make cleancore
	make all

# TAGS

TAGS:
	find . -name '*.lisp' | xargs etags -a

