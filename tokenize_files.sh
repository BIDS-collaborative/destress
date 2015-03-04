#!/bin/bash 

# This script will run xmltweet seperately on all files in $INDIR, outputting the tokenized representations and dictionaries to $OUTDIR. 
# Tokenizer must have the new behavior

readonly TOKENIZER=/home/anasrferreira/xmltweet2.exe #Location of tokenizer
readonly INDIR=/home/schillaci/SampleData/concatenated
readonly OUTDIR=/home/schillaci/SampleData/tokenized

for dat in $(ls ${INDIR}/*.xml); # Loop over all .xml files
do
    filename=${dat##*/}
    echo $(time ${TOKENIZER} -i ${dat} -o ${OUTDIR}/ -d ${OUTDIR}/${filename//.xml/}_dict ) # Runs xmltweet on all the .xml files and echoes the time taken
done



