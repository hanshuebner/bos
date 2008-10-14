#!/bin/sh

export PATH=/bin:/usr/bin:/usr/local/bin
CERTS=/home/bknr/certs
CHECKOUT=/home/bknr/bknr-svn/projects/bos
hostname=`hostname`

cd $HOME

while true
do
    sh $CHECKOUT/gen-cert.sh
    mail -s "Cert daemon on $hostname crashed" hans < /dev/null
    sleep 5
done