#!/bin/bash

set -e

nuweb -r main.w
pdflatex main.tex
