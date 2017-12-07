# simulate.py
# Frank Mei, Lisa Jian, Michael Jetsupphasuk, My Dinh
# 04 December, 2017
# Simulates water and earthquake data to test the power of lag-adjusted Spearman's

"""
Context: 

Fill me in!

"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from scipy.stats import rankdata
from scipy.stats import pearsonr
from datetime import datetime

import sim_functions as sim
from get_eq import get_eq
import csv
import random


##############################################################################

## GET TIME TO RUN SCRIPT
starttime = datetime.now()

##############################################################################

## LOAD IN REAL WATER DATA

# `water` is a list of lists with each internal list representing water
# injections from 1980-2017 (length 450) for a distinct block (87 blocks
# in total). 

water = []
with open('../data/final_water.csv') as file:
    f = csv.reader(file, delimiter = ',')
    next(f)
    for row in f:
        row = [float(r) for r in row]
        water.append(row)

# pick a random grid to take water injection from
# random.seed(157157)
random_water = random.sample(range(len(water)), 1)[0]
use_water = np.array(water[random_water])

##############################################################################

# ## SIMULATE AND PLOT

# # generate earthquake data
# # np.random.seed(157157)
eqs = get_eq(use_water)
dates = np.arange('1980-01', '2017-07', dtype='datetime64[M]')

# # plot water and earthquake data
plt.plot(dates, use_water)
plt.xticks(dates[::60])
plt.xlabel('dates')
plt.ylabel('bbl')
plt.savefig('water_injections.png')
plt.show()

plt.plot(dates, eqs)
plt.xticks(dates[::60])
plt.xlabel('dates')
plt.ylabel('counts')
plt.savefig('earthquakes.png')
plt.show()

# # rank data
eqs_rank = rankdata(eqs)
water_rank = rankdata(use_water)

# # run simulation
pval = sim.corr_test(water_rank, eqs_rank, norm = 2, plot = True, filename='corr_p.png')
print("P-value: ", pval)

##############################################################################

# ## TEST UNCORRELATED DATA

eqs = np.tile(np.array([0]*56+[10]),8)
eqs = np.array([np.exp(np.random.randn(1) + (4 if i % 50 == 0 else 0)) for i in range(450)])

# rank data
eqs_rank = rankdata(eqs)

plt.plot(dates, eqs)
plt.xticks(dates[::60])
plt.xlabel('dates')
plt.ylabel('counts')
plt.savefig('earthquakes_uncorr.png')
plt.show()

# run simulation
pval = sim.corr_test(water_rank, eqs_rank, norm = 2, plot = True, filename='uncorr_p.png')
print("P-value: ", pval)


##############################################################################

# ## PICK P-NORM

# with open('data/pnorm_pval.csv', 'w', newline='') as file:

#     norms_totry = np.array([2, 5, np.inf])
#     filewriter = csv.writer(file, delimiter=',')
#     filewriter.writerow(norms_totry)

#     for i in range(25):

#         # random pick of water data
#         random_water = random.sample(range(len(water)), 1)[0]
#         use_water = np.array(water[random_water])

#         # generate earthquake data
#         eqs = get_eq(use_water)

#         # rank data
#         eqs_rank = rankdata(eqs)
#         water_rank = rankdata(use_water)

#         # run simulation for the 4 values of p_norm
#         pvalues = []

#         for n in norms_totry:
#             p = sim.corr_test(water_rank, eqs_rank, norm = n, plot = False)
#             pvalues.append(p)

#         # write to csv file
#         filewriter.writerow(pvalues)


##############################################################################

# ## LOOK AT POWER OF MAX_LAGS
'''
with open('../data/lags_pvals.csv', 'w', newline='') as file:

    lags = np.array([0, 3, 6, 9, 12])
    filewriter = csv.writer(file, delimiter=',')
    filewriter.writerow(lags)

    for i in range(50):

        # random pick of water data
        random_water = random.sample(range(len(water)), 1)[0]
        use_water = np.array(water[random_water])

        # generate earthquake data
        eqs = get_eq(use_water)

        # rank data
        eqs_rank = rankdata(eqs)
        water_rank = rankdata(use_water)

        # run simulation for the 4 values of p_norm
        pvalues = []

        for n in lags:
            p = sim.corr_test(water_rank, eqs_rank, lag = n, norm = 1, plot = False)
            pvalues.append(p)

        # write to csv file
        filewriter.writerow(pvalues)
'''

##############################################################################

## GET TIME TO RUN SCRIPT

print(datetime.now() - starttime)


