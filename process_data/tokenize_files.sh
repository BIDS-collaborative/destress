#!/bin/bash 

# This script will run xmltweet seperately on all files in $INDIR, outputting the tokenized representations and dictionaries to $OUTDIR. 
# Tokenizer must have the new behavior

readonly TOKENIZER=$(dirname $0)/xmltweet.exe  # tokenizer path
readonly INDIR=/var/local/destress/combined
readonly OUTDIR=/var/local/destress/tokenized2
readonly FILELIST=$OUTDIR/fileList.txt

mkdir -p $OUTDIR
rm -f $FILELIST # This script always overwrites FILELIST

for dat in $(ls -v ${INDIR}/*.xml); # Loop over all .xml files
do
    filename=${dat##*/}
    echo $( ${TOKENIZER} -i ${dat} -o ${OUTDIR}/ -d ${OUTDIR}/${filename//.xml/}_dict ) # Runs xmltweet on all the .xml files
    echo ${filename//.xml/} >> $FILELIST # Write the two letter filename to a list for dictionary merge function in utils.scala
done



