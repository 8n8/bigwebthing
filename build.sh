#!/bin/bash

set -e

mkdir -p generated/server
nuweb -r main.w
pdflatex main.tex

pushd generated
bash build.sh
popd
