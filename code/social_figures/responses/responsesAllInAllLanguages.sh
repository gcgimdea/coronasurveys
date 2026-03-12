#!/bin/bash

#for f in ../../../data/PlotData/*.csv; do
#    country=`basename $f|sed 's/-esti.*//g'`

#    for langdir in `ls -d locales/*/`; do
    #	language=`basename $langdir`
#for language in en; do

find locales -maxdepth 1 -mindepth 1 -type d | \
while read -r language
do
    language=$(basename "$language")
	echo "doing  $language"
    
	LC_ALL=$language ./aggregate_plot.py -s -0.15 -t 0.05 -ct FR 
	LC_ALL=$language ./aggregate_plot.py -s -0.1 -ct ES
	LC_ALL=$language ./aggregate_plot.py -s -0.1 -t 0.1 -ct IT
	LC_ALL=$language ./aggregate_plot.py -s -0.1 -ct PT 
	LC_ALL=$language ./aggregate_plot.py -s -0.2 -ct CY 
	LC_ALL=$language ./aggregate_plot.py -s -0.1 -ct US 
	
done

