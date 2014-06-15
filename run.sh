#!/bin/sh

while true
do
    sbcl --no-userinit --load build.lisp --eval '(start)'
    mail -s 'BOS crashed' < /dev/null hans.huebner@gmail.com
    sleep 60
done
