#!/usr/bin/env python3


import sys
import os
import argparse

import pandas as pd

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as mticker
import seaborn as sns
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()

import gettext

## Arguments / Options

parser = argparse.ArgumentParser(description="Builds a social network friendly response status curve.")
parser.add_argument("-l", "--locale", help="the locale to choose the text from", default="en")
parser.add_argument("-c", "--country_code", help="the country code the data belongs to", default="WW")
parser.add_argument("-s", "--shift_contrib_x", type=float, help="how much to shift the arrow text on the x axis", default=0)
parser.add_argument("-t", "--shift_contrib_y", type=float, help="how much to shift the arrow text on the y axis", default=0)
parser.add_argument("-o", "--output", help="output image location")
parser.add_argument("csv_filename", help="the csv file to process", default="overall.csv", nargs="?")
args = parser.parse_args()
    
## Language

locale = gettext.translation('overall_plot', localedir='locales')
locale.install()
_ = locale.gettext

## Preprocessing

df = pd.read_csv(args.csv_filename)
df['date'] = pd.to_datetime(df['date'])
df['answers'] = df['answers'].cumsum()

## Plot Settings

figcolor=(40/255, 87/255, 128/255)
#figcolor=(27/255, 54/255, 93/255)
#figcolor=(28/255, 66/255, 32/255)
#figcolor=(63/255,42/255,86/255)
sns.set(rc={'axes.facecolor': figcolor,
            'figure.facecolor': figcolor,
            'grid.color': 'black',
            'lines.linewidth': 3,
            'grid.linestyle': '--',
            'savefig.facecolor': figcolor,
            'axes.edgecolor': figcolor,
            'axes.labelcolor': 'white',
            'xtick.color': 'white',
            'ytick.color': 'white',
            'xtick.labelsize': 20,
            'ytick.labelsize': 20,
            'font.family': 'Gotham Rounded'})
sns.set_palette(sns.light_palette((210, 90, 60), input="husl"))
fig, ax = plt.subplots(figsize=(10,10))

## Line Plot

snsplot = sns.lineplot(data=df, x='date', y='answers', ax=ax)

## Margins
plt.subplots_adjust(left=0.1, right=.95, top=0.9, bottom=0.2)

## Axes tikcks and tick labels

plt.locator_params(axis='y', nbins=3)
ax.xaxis.set_major_locator(mdates.WeekdayLocator(byweekday=(mdates.MO)))
#ax.xaxis.set_major_locator(mdates.DayLocator(bymonthday=range(5,32,5)))
ax.xaxis.set_major_formatter(mdates.DateFormatter('%B %dth'))

## Axes labels

ax.set_xlabel(_('Want to help? Complete the survey at \n coronasurveys.org!'),
              labelpad=30, fontsize=28, fontstyle='italic', fontname='Futura LT')
ax.set_ylabel('')

ax.set_xlim([df.loc[df.index[1], 'date'], df.loc[df.index[-1], 'date']])


## Title

if args.country_code == 'WW':
    title_subset = _('in the World')
elif args.country_code == 'AR':
    title_subset = _('in Argentina')
elif args.country_code == 'CL':
    title_subset = _('in Chile')
elif args.country_code == 'CY':
    title_subset = _('in Cyprus')
elif args.country_code == 'DE':
    title_subset = _('in Germany')
elif args.country_code == 'EC':
    title_subset = _('in Ecuador')
elif args.country_code == 'ES':
    title_subset = _('in Spain')
elif args.country_code == 'FR':
    title_subset = _('in France')
elif args.country_code == 'GB':
    title_subset = _('in the United Kingdom')
elif args.country_code == 'GR':
    title_subset = _('in Greece')
elif args.country_code == 'IT':
    title_subset = _('in Italy')
elif args.country_code == 'JP':
    title_subset = _('in Japan')
elif args.country_code == 'NL':
    title_subset = _('in the Netherlands')
elif args.country_code == 'PT':
    title_subset = _('in Portugal')
elif args.country_code == 'US':
    title_subset = _('in the USA')

ax.set_title(_('CoronaSurveys Responses ') + title_subset,
             color='white', size=30, fontweight='bold', fontname='Futura LT', pad=20)

## Arrow for Welcomed Contributions

ax.annotate(_('You can contribute here\nwith your responses!'),
            xy=(df.iloc[-2, 1], df.iloc[-1, 0]), xycoords='data',
            xytext=(0.7 + args.shift_contrib_x, 0.8 + args.shift_contrib_y), textcoords='axes fraction',
            arrowprops=dict(facecolor='white', connectionstyle="arc3,rad=-0.2"),
            horizontalalignment='right', verticalalignment='center_baseline',
            fontfamily='Futura LT', fontsize=28, color='white')

## Save the figure

if args.output:
    fig.savefig(args.output, dpi=200)
else:
    fig.savefig(os.path.splitext(args.csv_filename)[0] + '.jpg', dpi=200)
