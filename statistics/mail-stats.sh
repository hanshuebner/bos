#!/bin/sh

FILE=/tmp/contracts.xml
URL=http://127.0.0.1:8080/reports-xml/all-contracts
FROM='MXM Automatic Mailer <mxm-automail@createrainforest.org>'
TO='MXM Administrator <mxm-administrator@createrainforest.org>'
SUBJECT='Square meter contract database'

fetch -q -o $FILE $URL
perl -MMIME::Lite -e "MIME::Lite->new(From => '$FROM', To => '$TO', Subject => '$SUBJECT', Type => 'application/vnd.ms-excel', Path => '$FILE')->send()"
rm $FILE
