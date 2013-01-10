#!/bin/sh

while true
do
    make start
    mail -s 'BOS crashed' < /dev/null hans.huebner@gmail.com
    sleep 60
done