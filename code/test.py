# test.py
# Frank Mei, Lisa Jian, Michael Jetsupphasuk, My Dinh
# 05 December, 2017
# Applies lag-adjusted Spearman's to real water injection and earthquake data

"""
Context: 

We apply our test onto the real data and report our results. This script takes
in one command line argument: "ca" or "ok" for testing the California or 
Oklahoma data, respectively. Additionally, the arguments "ca_e1", "ca_n1", and
"ca_ne1" can be used to test California data shifted east, north, or northeast;
respectively.

"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from scipy.stats import rankdata
from scipy.stats import pearsonr
from scipy.stats import combine_pvalues

import sim_functions as sim
import csv
import random
import sys
import re

##############################################################################

## HANDLE COMMAND LINE ARGUMENTS

if sys.argv[1] == 'ca':
    filestring_w = 'data/final_water.csv'
elif sys.argv[1] == 'ca_e1':
    filestring_w = 'data/final_water_east1.csv'
elif sys.argv[1] == 'ca_n1':
    filestring_w = 'data/final_water_north1.csv'
elif sys.argv[1] == 'ca_ne1':
    filestring_w = 'data/final_water_ne1.csv'
elif sys.argv[1] == 'ok':
    filestring_w = 'data/final_water_ok.csv'
else:
    raise NameError('Input valid command line argument')

filestring_e = re.sub('water', 'eqs', filestring_w)
filestring_b = re.sub('water', 'blocks', filestring_w)
filestring_b = re.sub('.csv', '.txt', filestring_b)
filestring_save = 'results/pval_blocks_' + sys.argv[1] + '.csv'

##############################################################################

## LOAD IN REAL WATER DATA

# `water` is a list of lists with each internal list representing water
# injections from 1980-2017 (length 456) for a distinct block (87 blocks
# in total). 

water = []
with open(filestring_w) as file:
    f = csv.reader(file, delimiter = ',')
    next(f)
    for row in f:
        row = [float(r) for r in row]
        water.append(row)

##############################################################################

## LOAD IN REAL EARTHQUAKE DATA

# `eqs` is a list of lists with each internal list representing earthquakes
# from 1980-2017 (length 456) for a distinct block (87 blocks in total). 

eqs = []
with open(filestring_e) as file:
    f = csv.reader(file, delimiter = ',')
    next(f)
    for row in f:
        row = [float(r) for r in row]
        eqs.append(row)


##############################################################################

## LOAD IN GRID IDs

f = open(filestring_b, 'r')
grids = f.read().splitlines()
f.close()


##############################################################################

## APPLY TEST TO EACH BLOCK

# zip water, earthquake, and grid data
water_eqs = list(zip(water, eqs, grids))

# apply test to every grid
pvalues = []
pvalues_lower = []
pvalues_upper = []
for block in water_eqs:
    w = rankdata(block[0])
    eq = rankdata(block[1])
    grid = block[2]
    pval, pval_lower, pval_upper = sim.corr_test(w, eq, plot = False, norm = 2, num_trials = 10000)
    if pval == 0:
        pval = 0.00001
    pvalues.append(pval)
    pvalues_lower.append(pval_lower)
    pvalues_upper.append(pval_upper)
    # print("Grid: ", grid)
    # print("P-value: ", pval)

# save results
with open(filestring_save, "w", newline = '') as file:
    filewriter = csv.writer(file, delimiter = ',')
    filewriter.writerow(["Grid", "P-value", "P-value lower bound", "P-value upper bound"])
    for i in range(len(pvalues)):
        filewriter.writerow([grids[i], pvalues[i], pvalues_lower[i], pvalues_upper[i]])

## combined pvalues (Fisher's Method)
# with open("results/pval_combined.csv", "w", newline = '') as file:
#     filewriter = csv.writer(file, delimiter = ',')
#     filewriter.writerow([combine_pvalues(pvalues)[1]])
# print("Combined p-value (Fisher's): ", combine_pvalues(pvalues)[1])

# print results
sig_blocks = np.sum(np.array(pvalues) <= 0.05) # .05 sig level
sig_blocks2 = np.sum(np.array(pvalues) <= 0.1) # .1 sig level
print("Percent of blocks 'significant' at 0.05: ", sig_blocks / len(pvalues))
print("Percent of blocks 'significant' at 0.10: ", sig_blocks2 / len(pvalues))





