#!/bin/bash
awk '{ if (NR == 1) { prev_line = "(fastpyll \x27" $0; } else { print prev_line; prev_line = $0; } } END { if (NR > 0) { print prev_line ")"; } }' | guile fastpyll.scm
