#!/bin/bash
# compare.sh A B — list the builds whose counts differ between two
# results.txt files produced by count.sh
key() { sort "$1" | awk '{ k = $1 "/" $2 "/" $3; $1 = $2 = $3 = ""; print k "|" $0 }'; }
join -t'|' <(key "$1") <(key "$2") |
  awk -F'|' '$2 != $3 { print $1; print "   before:" $2; print "   after: " $3 }'
