#!/bin/sh -e

set -x
(cd ../../thirdparty/cl-gd && make)
lisp -core cmucl.core -noinit -load load.lisp -load build.lisp
