# simulate.py
# Frank Mei, Lisa Jian, Michael Jetsupphasuk, My Dinh
# 
# Simulates water and earthquake data to test the power of lag-adjusted Spearman's correlation

"""
Context: 

This file takes the csv files generated from `simulate.py` and takes summary statistics of the
results which are printed to the console.

"""

import numpy as np
import csv

##############################################################################

# LOAD IN DATA AS DICTIONARY

pvals = {}
for i in range(1, 13):
	with open('sim_results/pvals{0}.csv'.format(i), 'r') as file:
		f = csv.reader(file, delimiter = ',')
		pvals["pvals{0}".format(i)] = []
		for row in f:
			row = [float(r) for r in row]
			pvals["pvals{0}".format(i)].append(row)
		pvals["pvals{0}".format(i)] = np.array(pvals["pvals{0}".format(i)]).transpose()

# SAVE SUMMARY STATISTICS

print_statements = [
	"Low seismicity, low water, 3 months",
	"High seismicity, low water, 3 months",
	"Low seismicity, high water, 3 months",
	"High seismicity, high water, 3 months",
	"Low seismicity, low water, 9 months",
	"High seismicity, low water, 9 months",
	"Low seismicity, high water, 9 months",
	"High seismicity, high water, 9 months",
	"No seismicity, low water, 3 months",
	"No seismicity, high water, 3 months",
	"Low seismicity, no water, 3 months",
	"High seismicity, no water, 3 months"
]

to_save = [["", "Mean p-values", "Mean p-values lower", "Mean p-values upper", "% of p-values <= 0.05"]]
for i in range(1, 13):
	pval = pvals["pvals{0}".format(i)]
	p = ["{:.3f}".format(np.mean(p)) for p in pval]
	to_save.append([print_statements[i-1],
		float(p[0]), 
		float(p[1]),
		float(p[2]),
		"{:.1f}".format(len(pval[0][pval[0] <= 0.05]) / len(pval[0]) * 100) + "%"])

# write to csv
with open("sim_results/pvals_meta.csv", 'w+', newline = '') as file:
            filewriter = csv.writer(file, delimiter = ',')
            for lst in to_save:
            	filewriter.writerow(lst)
