#!/bin/bash

#for f in ../../../data/PlotData/*.csv; do
#    country=`basename $f|sed 's/-esti.*//g'`

#    for langdir in `ls -d locales/*/`; do
    #	language=`basename $langdir`
for language in en fr it; do
	echo "doing  $language"
	
	scale='t'
	LC_ALL=$language ./estimates_plot.py -ct FR -f 50  -o 10 -cs 75 -ccfr 20 -ccfryoff .1 -csyof 0.1 -cstof 30 -ccfrtof 35  -helpusoff 0 --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct ES -f 50  -o 10 -cs 55 -ccfr 20 -ccfryoff .1 -cstof -5 -helpusoff 0 --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct IT -f 50 -o 3 -cs 55 -ccfr 10 -ccfryoff .1 -cstof -5 -helpusoff 0 --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct PT -f 1 -o 3 -cs 45 -ccfr 15 -ccfryoff .1 -helpusoff 0 --cosurerror  --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct CY -f 10 -o 2 -cs 30 -ccfr 5 -ccfryoff 0.05 -ccfrtof 6 -cstof 0 -csyoff 0.07 -helpusoff 0 -oyoff 0.01 --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct GB -f 60 -o 10 -cs 47 -ccfr 20 -ccfryoff .1 -cstof -4 -helpusoff 0  --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct US -f 60 -o 5 -cs 47 -ccfr 15 -ccfryoff 0.1 -cstof -5 -oyoff 0.07 -helpusoff 0  --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct UA -f 1  -o 5 -cs 35 -ccfr 15 -ccfryoff 0.1 -ccfrtof 4 -cstof -5 -helpusoff 0  --cosurerror --logyscale $scale

	scale='f'

	
	LC_ALL=$language ./estimates_plot.py -ct FR -f 50  -o 10 -cs 75 -ccfr 20 -ccfrtof 2 -cstof 15 -ccfryoff 900000 -csyof 1000000 -helpusoff 95 -oyoff 300000 --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct ES -f 50  -o 10 -cs 40 -ccfr 30  -cstof -5 -ccfryoff 1000000 -csyof 1000000 -helpusoff 40 -oyoff 2000000 --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct IT -f 50 -o 3 -cs 50 -ccfr 35 -ccfrtof -18 -cstof -5 -ccfryoff 500000 -csyof 0000000 -helpusoff 0 -oyoff 1000000 --cosurerro --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct PT -f 1 -o 3 -cs 30 -ccfr 25 -ccfrtof -10  -ccfryoff 70000 -csyof 100000 -helpusoff 160 -oyoff 30000 --cosurerror  --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct CY -f 10 -o 2 -cs 12 -ccfr 5 -cstof 6 -ccfrtof 6  -ccfryoff 7000 -csyof 10000 -helpusoff 75 -oyoff 10000 --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct GB -f 60 -o 10 -cs 30 -ccfr 30 -ccfrtof -10  -cstof -2 -ccfryoff 900000 -csyof 700000 -helpusoff 60 -oyoff 300000 --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct US -f 60 -o 30 -cs 30 -otof -20 -ccfr 35  -cstof 3 -ccfrtof -10 -ccfryoff 2000000 -helpusoff 80 -csyof 3000000 -oyoff 1000000 --cosurerror --logyscale $scale
#	LC_ALL=$language ./estimates_plot.py -ct UA -f 1  -o 10 -cs 20 -otof -38  -ccfr 15  -ccfrtof 4 -cstof -5 -ccfryoff 200000 -helpusoff 10 -csyof 100000 -oyoff 300000 --cosurerror --logyscale $scale
done

