#!/bin/bash
awk '{ if (NR == 1) { prev_line = "(fastpyll_c_fastpyll_c \x27(\n\n" $0; } else { print prev_line; prev_line = $0; } } END { if (NR > 0) { print prev_line "\n))"; } }' | guile fastpyll_c.scm 
