#!/bin/sh

export PATH=/bin:/usr/bin:/usr/local/bin
HOME=/home/bknr/certs
hostname=`hostname`

cd $HOME

while true
do
    sh gen-cert.sh
    mail -s "Cert daemon on $hostname crashed" hans < /dev/null
    sleep 5
done