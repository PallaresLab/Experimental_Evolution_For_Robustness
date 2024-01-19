#!/bin/bash    

python3 preprocessing_conversion_command.py -i $1 -c $2

mkdir  $1_processed
mv train  $1_processed/
mv converted.xml ./$1_processed/$1.xml
