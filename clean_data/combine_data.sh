#!/usr/bin/env bash

# meant to be run on mercury.dlab.berkeley.edu

DATA_PATH=/var/local/destress/lj-annex/data/events
OUTPUT_PATH=/var/local/destress/combined3/events

MAX_FILE_SIZE=104857600 # 100Mb

mkdir -p $OUTPUT_PATH

# Loop over directories in DATA_PATH
for d in `ls $DATA_PATH`; do
    echo $d
    OUT_FILE_COUNTER=1 # Counter to track when files are split
    for f in `ls $DATA_PATH/$d/*.xml`; do
        if [ "$OUT_FILE_COUNTER" -eq 1 ]
          then
            OUT_FILE=$OUTPUT_PATH/$d.xml
          else
            OUT_FILE=$OUTPUT_PATH/$d$OUT_FILE_COUNTER.xml
        fi
        cat $f >> $OUT_FILE # Add to combined file

	# Split files if too large
        FILE_SIZE=$(stat -c%s $OUT_FILE)
        if (( "$FILE_SIZE" > "$MAX_FILE_SIZE" ))
          then
           OUT_FILE_COUNTER=$((OUT_FILE_COUNTER+1))
        fi
    done
done
