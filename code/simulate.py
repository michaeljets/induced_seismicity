# simulate.py
# Frank Mei, Lisa Jian, Michael Jetsupphasuk, My Dinh
# 04 December, 2017
# Simulates water and earthquake data to test the power of lag-adjusted Spearman's correlation

"""
Context: 

To test the validity of our proposed test statistic, we simulate synthetic earthquake
data (see `get_eq.py`) from real water injection data and apply our test statistic to
assess the utility of our test statistic given the parameters set forth in `get_eq.py`
which we vary in order to cover a variety of different cases.

This script takes in 4 arguments from the command line. In order, they are: the length
of time that earthquakes depend on previous water injections, the sum of the `beta`
coefficients, and `mu` as described in our synthetic seismicity function.

"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from scipy.stats import rankdata
from scipy.stats import pearsonr
from scipy.stats import expon
from datetime import datetime

import sim_functions as sim
from get_eq import get_eq

import sys
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
with open('data/final_water.csv') as file:
    f = csv.reader(file, delimiter = ',')
    next(f)
    for row in f:
        row = [float(r) for r in row]
        water.append(row)

# # pick a random grid to take water injection from
# # random.seed(157157)
# random_water = random.sample(range(len(water)), 1)[0]
# use_water = np.array(water[random_water])


# Specify one specific water block to use. 
use_water = np.array(water[81])

##############################################################################

### RUN PROGRAM X NUMBER OF TIMES

count = 1
while count <= 500:
    count += 1


##############################################################################

    ## GENERATE SYNTHETIC EARTHQUAKE DATA

    # construct appropriate betas for earthquake generation
    # `num_betas` and `total_betas` taken from command line
    assert int(sys.argv[1]) >= 1, \
        "First argument (num_betas) must be positive integer"
    num_betas = int(sys.argv[1])
    total_betas = float(sys.argv[2])
    bins = np.arange(0, 6, 6/num_betas)

    # `total_betas` is split across the betas according to an
    # exponential distribution.
    w = np.append(expon.cdf(bins, scale = 1/.5), 1)
    weights = []
    for i,j in zip(w, w[1:]):
        weights.append(j-i)
    b = np.array([total_betas] * len(weights)) * np.array(weights)

    # get mu from command line
    mu = float(sys.argv[3])

    # generate earthquake data
    # np.random.seed(157157)
    eqs = get_eq(use_water, betas = b, mu = mu)
    dates = np.arange('1980-01', '2017-07', dtype='datetime64[M]')

    ##############################################################################

    ## SIMULATE AND PLOT

    ## plot water data
    # plt.plot(dates, use_water)
    # plt.xticks(dates[::60])
    # plt.xlabel('dates')
    # plt.ylabel('bbl')
    # plt.savefig('water_injections.png')
    # plt.show()

    ## plot earthquake data
    # plt.plot(dates, eqs)
    # plt.xticks(dates[::60])
    # plt.xlabel('dates')
    # plt.ylabel('counts')
    # plt.savefig('earthquakes.png')
    # plt.show()

    rank data
    eqs_rank = rankdata(eqs)
    water_rank = rankdata(use_water)

    # run simulation
    pval, pval_lower, pval_upper = sim.corr_test(water_rank, eqs_rank, norm = 2, plot = False, filename='corr_p.png', num_trials = 5000)

    # write results
    filestring = 'sim_results/' + sys.argv[4]
    if count == 2:
        with open(filestring, 'w+', newline = '') as file:
            filewriter = csv.writer(file, delimiter = ',')
            filewriter.writerow([pval, pval_lower, pval_upper])
    else:
        with open(filestring, 'a', newline = '') as file:
            filewriter = csv.writer(file, delimiter = ',')
            filewriter.writerow([pval, pval_lower, pval_upper])


##############################################################################

## GET TIME TO RUN SCRIPT

print(datetime.now() - starttime)


