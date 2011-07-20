#!/bin/sh

export PATH=/bin:/usr/bin:/usr/local/bin
CERTS=/home/hans/certs
CHECKOUT=/home/hans/bos/cert-daemon
hostname=`hostname`

cd $CERTS

while true
do
    sh $CHECKOUT/cert-daemon.sh
    mail -s "Cert daemon on $hostname crashed" hans < /dev/null
    sleep 5
done
