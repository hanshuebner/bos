#!/bin/sh

export PATH=/bin:/usr/bin:/usr/local/bin
CERTS=/home/bknr/certs
CHECKOUT=/home/bknr/bknr-svn/projects/bos/cert-daemon
hostname=`hostname`

cd $CERTS

while true
do
    sh $CHECKOUT/cert-daemon.sh
    mail -s "Cert daemon on $hostname crashed" hans < /dev/null
    sleep 5
done
