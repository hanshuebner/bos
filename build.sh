#!/bin/sh -e

case "$1" in
	--clean)
		echo "deleting fasls... (use --fast to suppress)"
		find .. -name \*.x86f -print0 | xargs -0 rm
		;; 
	--fast)
		echo "not deleting fasls"
		;; 
	*)
		echo "error: expected argument --clean or --fast" 1>&2
        	exit 1
		;;
esac

pwd
set -x
(cd ../thirdparty/cl-gd && make)
lisp -core cmucl.core -noinit -load load.lisp -load build.lisp
