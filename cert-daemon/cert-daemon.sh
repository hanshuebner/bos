#!/bin/sh

set -e

gen_certs()
{
    contract=$1
    language=$2

    echo "generating certs for contract $contract, language $language"

    print_fdf_file=mail-spool/$contract-$language.fdf
    print_pdf_file=mail-spool/$contract.pdf
    download_fdf_file=download-spool/$contract-$language.fdf
    download_pdf_file=download-spool/$contract.pdf

    m2s_pdf_file=download-spool/$contract-m2s.pdf

    tmp1_file=/tmp/gen-cert-$$-1.pdf
    tmp2_file=/tmp/gen-cert-$$-2.pdf
    tmp3_file=/tmp/gen-cert-$$-3.pdf
    tmp4_file=/tmp/gen-cert-$$-4.pdf

    trap "rm -f $tmp1_file $tmp2_file $tmp3_file $tmp4_file" EXIT

    if [ -f $print_fdf_file ]; then
        pdftk urkunde-print-$language.pdf fill_form $print_fdf_file output $tmp1_file flatten
        pdftk $tmp1_file cat 1 output $tmp2_file
        pdftk $tmp1_file cat 2 output $tmp3_file
        pdftk $m2s_pdf_file background $tmp3_file output $tmp4_file
        pdftk $tmp2ww_file $tmp4_file output $print_pdf_file
        echo generated $print_pdf_file
    fi

    pdftk urkunde-print-$language.pdf fill_form $download_fdf_file output $tmp1_file
    pdftk $m2s_pdf_file background $tmp1_file output $download_pdf_file
    echo generated $download_pdf_file
    rm -f $tmp1_file $tmp2_file $tmp3_file $tmp4_file $print_fdf_file $download_fdf_file
    trap "" EXIT

}

while true; do
    sleep 2
    if [ -f download-spool/*-*.fdf ]; then
        for job in download-spool/*-*.fdf
        do
          gen_certs `echo $job | perl -pe 's|.*-spool/(.*)-(.*).fdf|$1 $2|'`
        done
    fi
done