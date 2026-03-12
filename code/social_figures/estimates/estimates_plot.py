#!/usr/local/bin/python3

import sys
import os
import argparse

import pandas as pd
import numpy as np

import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.ticker as mticker
import matplotlib.patches as mpatches
import seaborn as sns

import gettext
import itertools

from datetime import date
from matplotlib.text import OffsetFrom
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()
## Arguments / Options

# parsing boolean arguments from https://stackoverflow.com/questions/15008758/parsing-boolean-values-with-argparse
def str2bool(v):
    if isinstance(v, bool):
       return v
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')
    

parser = argparse.ArgumentParser(description="Builds a social-network-friendly estimate plot.")
parser.add_argument("--ccfrerror", type=str2bool, nargs='?', const=True, default=False, help="Display ccfr error.")
parser.add_argument("--cosurerror", type=str2bool, nargs='?', const=True, default=False, help="Display coronasurveys error.")
parser.add_argument("--logyscale", type=str2bool, nargs='?', const=True, default=False, help="Use log scale for y axis.")
parser.add_argument("-l", "--locale", help="the locale to choose the text from", default="en")
parser.add_argument("-ct", "--country_code", help="the country code the data belongs to", default="WW")
parser.add_argument("-s", "--shift_contrib_x", type=float, help="how much to shift the arrow text on the x axis", default=0)
parser.add_argument("-f", "--first_datapoint", help="first datapoint to plot", type=int)
parser.add_argument("-o", "--official_arrow_datapoint", help="datapoint after the first plotted to point the arrow to for the official number", type=int, default=5)
parser.add_argument("-cs", "--cosur_arrow_datapoint", help="datapoint after the first plotted point to point the arrow for coronasurveys", type=int, default=10)
parser.add_argument("-ccfr", "--ccfr_arrow_datapoint", help="datapoint after the first plotted point to point the arrow for ccfr", type=int, default=10)
parser.add_argument("-otof", "--official_text_offset", help="text offset to for the official number", type=int, default=5)
parser.add_argument("-cstof", "--cosur_text_offset", help="text offset for coronasurveys", type=int, default=-7)
parser.add_argument("-ccfrtof", "--ccfr_text_offset", help="text offset for ccfr", type=int, default=15)
parser.add_argument("-helpusoff", help="text offset for ccfr", type=int, default=60)
parser.add_argument("-ccfryoff", "--ccfrEstimateYOffset", help="multiplicative/additive factor for ccfr estimate to place corresponding text", type=float, default=0.001)
parser.add_argument("-csyoff", "--cosurEstimateYOffset", help="multiplicative/additive factor for coronasurveys estimate to place corresponding text", type=float, default=.33)
parser.add_argument("-oyoff", "--officialYOffset", help="multiplicative/additive factor for official number to place corresponding text", type=float, default=1)
#parser.add_argument("csv_filename", help="the csv file to process", default="../../../data/aggregate/FR-aggregate.csv", nargs="?")
args = parser.parse_args()

if args.first_datapoint:
    args.official_arrow_datapoint += args.first_datapoint
    args.cosur_arrow_datapoint+=args.first_datapoint
    args.ccfr_arrow_datapoint+=args.first_datapoint
#print(args.official_arrow_datapoint)

## Language

locale = gettext.translation('estimates_plot', localedir='locales')
locale.install()
_ = locale.gettext


## Preprocessing

filename="../../../data/PlotData/"+args.country_code+"-estimates.csv"
df = pd.read_csv(filename)
df['date'] = [pd.to_datetime(d, errors='ignore') for d in df['date']]
#print(df['date'])
#print (df['cum_cases'])
#print (df['est_ccfr'])
#print (df['estimated_cases'])

ccfrisgtzero=(df['est_ccfr'] > 0)

#df['date'] = pd.to_datetime(df['date'])
#df['answers'] = df['answers'].cumsum()

#print (df)

#exit
## Plot Settings

#figcolor=(40/255, 87/255, 128/255)
#figcolor=(27/255, 54/255, 93/255)
#figcolor=(28/255, 66/255, 32/255)
#figcolor=(63/255, 42/255, 86/255)
figcolor=(93/255, 62/255, 128/255)
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

fig, ax = plt.subplots(figsize=(10,10))
if args.logyscale:
    print ("using log scale")
    ax.set_yscale("log")
    scalesuffix="-log"
else:
    print ("using linear scale")
    scalesuffix="-linear"


#### prepare error bands
#interpolated=df.resample('1D')
interpolated=df.copy()
df['estimate_cases_low']=df['estimate_cases_low'].apply(lambda x: 1 if x<1 else x) #replace negative values by 1
df['est_ccfr_low']=df['est_ccfr_low'].apply(lambda x: 1 if x<1 else x) #replace negative values by 1

interpolated['estimate_cases_low']=df['estimate_cases_low'].interpolate()
interpolated['estimate_cases_high']=df['estimate_cases_high'].interpolate()

interpolated['est_ccfr_low']=df['est_ccfr_low'].interpolate()
interpolated['est_ccfr_high']=df['est_ccfr_high'].interpolate()

#print ("DF")
#print (df['estimate_cases_low'])
#print ("Interpolated")
#print(interpolated['estimate_cases_low'])

########


#palette = sns.color_palette(palette=sns.crayon_palette(sns.colors.crayons))
palette = sns.color_palette('colorblind') #sns.light_palette((210, 90, 60), input="husl"))
#sns.set_palette(palette)
new_palette = itertools.cycle(palette)
next(new_palette)

## Line Plots

## Official Estimate
print ("plotting official")

next_color=palette[7]#next(new_palette)
snsplot = sns.lineplot(data=df, x='date', y='cum_cases', ax=ax, color=next_color)

#print (args.official_arrow_datapoint)
#print (df.index[args.official_arrow_datapoint])
#print (df.loc[df.index[args.official_arrow_datapoint], 'date'])

official_arrow_x = df.loc[df.index[args.official_arrow_datapoint], 'date']
official_arrow_y = df.loc[df.index[args.official_arrow_datapoint], 'cum_cases']
official_arrow_text_x = df.loc[df.index[args.official_arrow_datapoint + args.official_text_offset], 'date']
if args.logyscale:
    official_arrow_text_y = df.loc[df.index[args.official_arrow_datapoint], 'cum_cases'] * args.officialYOffset
else:
    official_arrow_text_y = df.loc[df.index[args.official_arrow_datapoint], 'cum_cases'] + args.officialYOffset
    
ax.annotate(_('Confirmed cases'),
            xy=(official_arrow_x, official_arrow_y), xycoords='data',
            xytext=(official_arrow_text_x, official_arrow_text_y), textcoords='data',
            arrowprops=dict(arrowstyle='fancy', connectionstyle="arc3,rad=-0.2",
                            facecolor=next_color, edgecolor=next_color,
                            shrinkA=5, shrinkB=5,
                            relpos=(0, 0.5)),
            horizontalalignment='left', verticalalignment='center_baseline',
            fontfamily='Futura LT', fontsize=28, color=next_color)

## Estimate from Death Rate
print ("plotting ccfr")

next_color=next(new_palette)
sns.lineplot(data=df[ccfrisgtzero], x='date', y='est_ccfr', ax=ax, color=next_color)
#draw errobands
if args.ccfrerror:
    ax.fill_between(df['date'], interpolated['est_ccfr_low'], interpolated['est_ccfr_high'],facecolor=next_color,   alpha=0.2) #,  interpolate=True) #where=df['estimate_cases_low'] < df['estimate_cases_high'],



#print (args.ccfr_arrow_datapoint)
#print (df.index[args.ccfr_arrow_datapoint])
#print (df.loc[df.index[args.ccfr_arrow_datapoint], 'date'])

ccfr_estimate_arrow_x = df.loc[df.index[args.ccfr_arrow_datapoint], 'date']
ccfr_estimate_arrow_y = df.loc[df.index[args.ccfr_arrow_datapoint], 'est_ccfr']
ccfr_estimate_arrow_text_x = df.loc[df.index[args.ccfr_arrow_datapoint + args.ccfr_text_offset], 'date']
if args.logyscale:
    ccfr_estimate_arrow_text_y = df.loc[df.index[args.ccfr_arrow_datapoint], 'est_ccfr'] * args.ccfrEstimateYOffset
else:
    ccfr_estimate_arrow_text_y = df.loc[df.index[args.ccfr_arrow_datapoint], 'est_ccfr'] + args.ccfrEstimateYOffset
    
ax.annotate(_('Estimated cases based\non 1.4% death-rate')+'$^*$',
            xy=(ccfr_estimate_arrow_x, ccfr_estimate_arrow_y), xycoords='data',
            xytext=(ccfr_estimate_arrow_text_x, ccfr_estimate_arrow_text_y ), textcoords='data',
            arrowprops=dict(arrowstyle='fancy',connectionstyle="arc3,rad=-0.4",
                            facecolor=next_color,edgecolor=next_color,
                            shrinkA=5,shrinkB=5,
                            relpos=(0.5, 1)),
            horizontalalignment='center', verticalalignment='top', multialignment='left',
            fontfamily='Futura LT', fontsize=28, color=next_color)

ccfrcolor=next_color
## CoronaSurveys Estimate
#sns.lineplot(data=df, x='date', y='estimated_cases', ax=ax, color=next(new_palette))

#next(new_palette)
next_color=next(new_palette)
sns.lineplot(data=df, x='date', y='estimated_cases', ax=ax, color=next_color) #, err_style="bars")

#draw errobands
if args.cosurerror:
    ax.fill_between(df['date'], interpolated['estimate_cases_low'], interpolated['estimate_cases_high'],facecolor=next_color,   alpha=0.2) #,  interpolate=True) #where=df['estimate_cases_low'] < df['estimate_cases_high'],


ix=args.cosur_arrow_datapoint
while ix < len(df.index) and np.isnan(df.loc[df.index[ix], 'estimated_cases']):
   ix += 1


print ("plotting coronasurveys")
#print (ix)
#print (df.index[ix])
#print (df.loc[df.index[ix], 'date'])

cosur_estimate_arrow_x = df.loc[df.index[ix], 'date']
cosur_estimate_arrow_y = df.loc[df.index[ix], 'estimated_cases']
cosur_estimate_arrow_text_x = df.loc[df.index[ix + args.cosur_text_offset], 'date']
if args.logyscale:
    cosur_estimate_arrow_text_y = df.loc[df.index[ix], 'estimated_cases'] * args.cosurEstimateYOffset
else:
    cosur_estimate_arrow_text_y = df.loc[df.index[ix], 'estimated_cases'] + args.cosurEstimateYOffset
    
#print (cosur_estimate_arrow_x, cosur_estimate_arrow_y, cosur_estimate_arrow_text_x, cosur_estimate_arrow_text_y)
ancosur=ax.annotate(_('Estimated cases based\non CoronaSurveys'),
            xy=(cosur_estimate_arrow_x, cosur_estimate_arrow_y), xycoords='data',
            xytext=(cosur_estimate_arrow_text_x, cosur_estimate_arrow_text_y), textcoords='data',
            arrowprops=dict(arrowstyle='fancy',connectionstyle="arc3,rad=0.4",
                            facecolor=next_color,edgecolor=next_color,
                            shrinkA=5,shrinkB=5,
                            relpos=(1, 1)),
            horizontalalignment='right', verticalalignment='top', multialignment='left',
            fontfamily='Futura LT', fontsize=28, fontweight='bold', color=next_color)

if args.logyscale:
    helpusY=cosur_estimate_arrow_text_y / 2
else:
    helpusY=cosur_estimate_arrow_text_y - (ax.get_ylim()[1]-ax.get_ylim()[0])/10
#ax.annotate(_('help us get more data'),
#            xy=(cosur_estimate_arrow_text_x, helpusY ), xycoords='data',
#            horizontalalignment='left', verticalalignment='top', multialignment='left',
#            fontfamily='Futura LT', fontsize=20, fontweight='normal', color=next_color)


offset_from = OffsetFrom(ancosur, (0, 0))
yoff= args.helpusoff
#print ("yoff=",yoff)
ax.annotate(_('help us get more data'),
            xy=(0, yoff), xycoords=offset_from,
            horizontalalignment='left', verticalalignment='top', multialignment='left',
            fontfamily='Futura LT', fontsize=20, fontweight='normal', color=next_color)




#sns.lineplot(data=df, x='date', y='prop_cases', ax=ax, color=next(new_palette)) #, err_style="bars")

#sns.lineplot(data=df, x='date', y='dunbar_cases', ax=ax) #, err_style="bars")

## Date Limits
if args.first_datapoint:
    ax.set_xlim([df.loc[df.index[args.first_datapoint], 'date'], df.loc[df.index[-1], 'date']])
else:
    ax.set_xlim([df.loc[df.index[1], 'date'], df.loc[df.index[-1], 'date']])
## Margins
plt.subplots_adjust(left=0.1, right=.9, top=0.9, bottom=0.25)


## Axes tikcks and tick labels

@mticker.FuncFormatter
def my_ytick_formatter(x, pos):
    if x>0:
        tmp = np.log10(x)
    else:
        tmp=0

    if tmp < 3:
        return str(int(x))
    elif tmp < 6:
        return str(int(x / 1000)) + 'k'
    else:
        if args.logyscale:
            return str(int(x / 1000000)) + 'M'
        else:
            return str(float(int(x / 100000))/10) + 'M'



#plt.locator_params(axis='y', nbins=3)
ax.xaxis.set_major_locator(mdates.AutoDateLocator()) # WeekdayLocator(byweekday=(mdates.MO))) #DayLocator(bymonthday=range(5,32,10)))
plt.xticks(rotation=20)
ax.xaxis.set_major_formatter(mdates.DateFormatter('%b %dth'))
ax.yaxis.set_major_formatter(my_ytick_formatter)

## Axes labels

xlabel=ax.set_xlabel(_('Want to help? Complete the survey at \n coronasurveys.org!'),
              labelpad=30, fontsize=28, fontstyle='italic', fontname='Futura LT')

#offset_from_xlabel = OffsetFrom(xlabel, (0, 0))
ax.set_ylabel('')

#ax.annotate('$^*$'+_('estimation based on ')+'https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html',
#            xy=(280, -10), xycoords='offset_from_xlabel,
#            horizontalalignment='center', verticalalignment='top', multialignment='center',
#            fontfamily='Futura LT', fontsize=14, fontweight='normal', color=ccfrcolor)

ax.annotate('$^*$'+_('estimation based on ')+'https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html',
            xy=(280, -160), xycoords='axes points',
            horizontalalignment='center', verticalalignment='top', multialignment='left',
            fontfamily='Futura LT', fontsize=14, fontweight='normal', color=ccfrcolor)

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
elif args.country_code == 'UA':
    title_subset = _('in Ukraine')

ax.set_title(_('Covid-19 Cases Estimates ') + title_subset,
             color='white', size=29, fontweight='bold', fontname='Futura LT', pad=20)

## Arrow for Welcomed Contributions

#ax.annotate(_('You can contribute here\nwith your responses!'),
            #xy=(df.loc[df.index[-2], 'date'], df.loc[df.index[-10], 'estimated_cases'] / 2), xycoords='data',
            #xytext=(0.4 + args.shift_contrib_x, 0.9), textcoords='axes fraction',
            #arrowprops=dict(facecolor='white', connectionstyle="arc3,rad=-0.2"),
            #horizontalalignment='right', verticalalignment='center_baseline',
            #fontfamily='Futura LT', fontsize=28, color='white')

## Save the figure

#fig.savefig(os.path.splitext(filename)[0] + '.jpg', dpi=200)

#print os.path.split(filename)
outfilename='../../../Plots/socialPlots/estimates/'+os.path.splitext(os.path.split(filename)[1])[0] +'-'+locale.info()['language']+'-'+ str(date.today())+scalesuffix+'.jpg'
outfilename2='../../../Plots/socialPlots/estimates/'+os.path.splitext(os.path.split(filename)[1])[0] +'-'+locale.info()['language']+'-'+ str(date.today())+scalesuffix+'2.jpg'
#print(outfilename)
#pd.plotting.register_matplotlib_converters()
fig.savefig(outfilename, dpi=200)

#print ("Bbox")
#bbox=ancosur.get_window_extent()
#print (bbox)
#rect=mpatches.Rectangle((50,100),279730,307375,linewidth=1,edgecolor='r',facecolor='none')
#mpatches.Rectangle((bbox.xmin, bbox.ymin), bbox.width, bbox.height, linewidth=4, fill=True, facecolor='white',edgecolor='white')
#Rectangle((100, 100), 100, 100, fill=True, facecolor='white',edgecolor='white', figure=fig)
#ax.add_patch(rect)


#plt.show()
#fig.savefig(outfilename2, dpi=200)
#print("xlim=",ax.get_xlim())
#print("ylim=",ax.get_ylim())

#print ("cosuryoff=",args.cosurEstimateYOffset)

#yoff=args.cosurEstimateYOffset/(ax.get_ylim()[1])*180
#amplitude=ax.get_ylim()[1]-ax.get_ylim()[0]
#print ("amplitude=",amplitude)
#fig.savefig(outfilename, dpi=200)

#ax.add_patch(bbox.as_artist(facecolor='none', edgecolor='white',
        #     lw=2.))
