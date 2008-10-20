#!/bin/sh

set -e

gen_certs()
{
    contract=$1
    language=$2

    echo "generating certs for contract $contract, language $language"

    print_fdf_file=mail-spool/$contract-$language.fdf
    print_m2s_pdf_file=mail-spool/$contract-m2s.pdf
    print_pdf_file=mail-spool/$contract.pdf

    download_m2s_pdf_file=download-spool/$contract-m2s.pdf
    download_fdf_file=download-spool/$contract-$language.fdf
    download_pdf_file=download-spool/$contract.pdf

    if [ $language = de -a -f $print_fdf_file ]; then
        pdftk $print_m2s_pdf_file fill_form $print_fdf_file output $print_pdf_file $flatten
        echo generated $print_pdf_file
    fi

    pdftk $download_m2s_pdf_file fill_form $download_fdf_file output $download_pdf_file $flatten
    echo generated $download_pdf_file
    rm -f $print_m2s_pdf_file $print_fdf_file $download_m2s_pdf_file $download_fdf_file
    trap "" EXIT

}

while true; do
    sleep 2
    set download-spool/*-*.fdf
    if [ -f $1 ]; then
        for job in download-spool/*-*.fdf
        do
          gen_certs `echo $job | perl -pe 's|.*-spool/(.*)-(.*).fdf|$1 $2|'`
        done
    fi
done
