#!/bin/sh

COMPILER=$1
SOURCE=$2
DESTINATION=$3

BUILD_PATH_PREFIX_MAP=/root=`pwd` \
$COMPILER --source-map --no-runtime --source-map-no-source \
$SOURCE -o $DESTINATION
