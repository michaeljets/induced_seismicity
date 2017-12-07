# test.py
# Frank Mei, Lisa Jian, Michael Jetsupphasuk, My Dinh
# 05 December, 2017
# Applies lag-adjusted Spearman's to real water injection and earthquake data

"""
Context: 

Fill me in!

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

##############################################################################

## LOAD IN REAL EARTHQUAKE DATA

# `eqs` is a list of lists with each internal list representing earthquakes
# from 1980-2017 (length 456) for a distinct block (87 blocks in total). 

eqs = []
with open('data/final_eqs.csv') as file:
    f = csv.reader(file, delimiter = ',')
    next(f)
    for row in f:
        row = [float(r) for r in row]
        eqs.append(row)


##############################################################################

## LOAD IN GRID IDs

f = open('data/final_blocks.txt', 'r')
grids = f.read().splitlines()


##############################################################################

## APPLY TEST TO EACH BLOCK

# zip water, earthquake, and grid data
water_eqs = list(zip(water, eqs, grids))

# apply test to every grid
pvalues = []
for block in water_eqs:
    w = rankdata(block[0])
    eq = rankdata(block[1])
    grid = block[2]
    pval = sim.corr_test(w, eq, plot = False, norm = 2)
    if pval == 0:
        pval = 0.00001
    pvalues.append(pval)
    print("Grid: ", grid)
    print("P-value: ", pval)

# save results
with open("data/pval_blocks.csv", "w", newline = '') as file:
    filewriter = csv.writer(file, delimiter = ',')
    for i in range(len(pvalues)):
        filewriter.writerow([pvalues[i], grids[i]])

with open("data/pval_combined.csv", "w", newline = '') as file:
    filewriter = csv.writer(file, delimiter = ',')
    filewriter.writerow([combine_pvalues(pvalues)[1]])

# print results
sig_blocks = np.sum(np.array(pvalues) <= 0.05)
print("Percent of blocks 'significant': ", sig_blocks / len(pvalues))
print("Combined p-value (Fisher's): ", combine_pvalues(pvalues)[1])




