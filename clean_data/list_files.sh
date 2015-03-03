#!/usr/bin/env bash

# meant to be run on mercury.dlab.berkeley.edu

DATA_PATH=/var/local/destress/lj-annex/data/events
OUTPUT_PATH=/home/pierre/combined
OUTPUT_NAME=files.txt

mkdir -p $OUTPUT_PATH

echo > $OUTPUT_PATH/$OUTPUT_NAME

for f in `ls $DATA_PATH`; do
    echo $f
    find -L $DATA_PATH/$f -maxdepth 1 -type f | sort  >> $OUTPUT_PATH/$OUTPUT_NAME
done

