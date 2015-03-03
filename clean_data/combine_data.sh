#!/usr/bin/env bash

# meant to be run on mercury.dlab.berkeley.edu

DATA_PATH=/var/local/destress/lj-annex/data/events
OUTPUT_PATH=/home/pierre/combined/events

mkdir -p $OUTPUT_PATH

for f in `ls $DATA_PATH`; do
    echo $f
    # cat $DATA_PATH/$f/* > $OUTPUT_PATH/$f.xml
    find -L $DATA_PATH/$f -maxdepth 1 -type f -print0 | sort -z | xargs -0 cat -- > $OUTPUT_PATH/$f.xml
done

