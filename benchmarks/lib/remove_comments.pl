#!/usr/bin/perl
$/ = undef;
$_ = <>;
s#\(\*[^*]*\*+([^)*][^*]*\*+)*\)#defined $2 ? $2 : ""#gse;
print;
