#! /bin/bash
set -e

find . -name '*.coverage' | xargs rm -f
dune runtest --instrument-with bisect_ppx --force
bisect-ppx-report html
open ./_coverage/index.html
