#!/bin/zsh -
Rscript app.R $1 |& awk '/http/ {system("open " $3)}'
