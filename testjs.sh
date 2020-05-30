#!/bin/sh
cp "$@" ../citeproc-js/fixtures/local/jgm_tmp.txt
cd ../citeproc-js
cslrun -s jgm_tmp

