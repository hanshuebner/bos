#!/bin/sh -e
cd `dirname $0`/..
lisp -noinit -core bos/build.core -load bos/build.lisp
