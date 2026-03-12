#!/bin/bash
echo "updating template"
xgettext --package-name=coronasurveys-social-plot --package-version=1 -d estimates_plot -o locales/estimates_plot.pot estimates_plot.py
sed -i bak 's/CHARSET/UTF-8/' locales/estimates_plot.pot

echo "updating locales"
pushd locales
find . -maxdepth 2 -mindepth 2 -type d -exec msgmerge -U {}/estimates_plot.po estimates_plot.pot \;

echo "compiling locales"
find . -maxdepth 2 -mindepth 2 -type d -exec msgfmt --check --verbose --output-file {}/estimates_plot.mo {}/estimates_plot.po \;
popd 
