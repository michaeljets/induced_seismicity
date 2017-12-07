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
import csv
import random

##############################################################################

## GET TIME TO RUN SCRIPT
starttime = datetime.now()

##############################################################################

## LOAD IN REAL WATER DATA

# `water` is a list of lists with each internal list representing water
# injections from 1980-2017 (length 456) for a distinct block (87 blocks
# in total). 

water = []
with open('data/final_water.csv') as file:
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

## EARTHQUAKE GENERATION FUNCTIONS

def get_eq_simple(water):
    """
    Simulates earthquake data with a poisson process with a dependency on water at lag 0.
    """

    eq = np.array([np.random.poisson(np.exp(np.random.randn(1))*(0.0000001+0.00001*water[i])) 
                   for i in range(len(water))])
    return eq


def get_eq_timelag(water):
    """
    Simulates earthquake data with a poisson process with a dependency on water at lag 0.
    Also includes a dependency on previous earthquake data.
    """

    eq = np.random.poisson(np.exp(np.random.randn(1))*(0.0000001+0.00001*water[0]))
    for i in range(1, len(water)):
        new = np.random.poisson(np.exp(np.random.randn(1))*(0.0000001+0.00001*water[i]) \
                                +np.exp(np.random.randn(1))*0.047*eq[i-1])
        eq = np.concatenate([eq, new])        
    return eq


def get_eq(water):
    """
    Simulates earthquake data with a poisson process with a dependency on water at lag 0-4.
    Also includes a dependency on previous earthquake data.
    """

    eq = np.random.poisson(np.exp(np.random.randn(1))*(0.0000001+0.00001*water[0]))
    for i in range(1, len(water)):
        if i < 4:
            new = np.random.poisson(np.exp(np.random.randn(1))*(0.0000001+0.00001*water[i]) + \
                                    np.exp(np.random.randn(1))*0.047*eq[i-1])
        else:
            new = np.random.poisson(np.exp(np.random.randn(1))*(0.0000001 + \
                                                                 np.random.uniform(1)*0.000001*water[i] + \
                                                                 np.random.uniform(1)*0.0000009*water[i-1] + \
                                                                 np.random.uniform(1)*0.0000007*water[i-2] + \
                                                                 np.random.uniform(1)*0.0000006*water[i-3] + \
                                                                 np.random.uniform(1)*0.0000006*water[i-4]) + \
                                    np.exp(np.random.randn(1))*0.047*eq[i-1])

        eq = np.concatenate([eq, new])
    return eq

##############################################################################

## SIMULATE AND PLOT

# generate earthquake data
# np.random.seed(157157)
eqs = get_eq(use_water)

# plot water and earthquake data
plt.plot(use_water)
plt.show()

plt.plot(eqs)
plt.show()

# rank data
eqs_rank = rankdata(eqs)
water_rank = rankdata(use_water)

# run simulation
pval = sim.corr_test(water_rank, eqs_rank, norm = 2, plot = True)
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

## GET TIME TO RUN SCRIPT

print(datetime.now() - starttime)


