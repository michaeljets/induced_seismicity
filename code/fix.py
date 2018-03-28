import numpy as np
import csv
from statsmodels.stats.proportion import proportion_confint

for i in range(1, 13):
	pvals = []
	with open('sim_results/pvals{0}.csv'.format(i), 'r') as file:
		f = csv.reader(file, delimiter = ',')
		for row in f:
			pvals.append(float(row[0]))

	x = proportion_confint(np.array(pvals)*5000, 5000, alpha = 0.05, method = 'beta')
	y = np.array(x).transpose()
	
	with open('sim_results/pvals{0}.csv'.format(i), 'w', newline = '') as file:
		f = csv.writer(file, delimiter = ',')
		for j in range(len(pvals)):
			lower = y[j][0]
			upper = y[j][1]
			if np.isnan(lower):
				lower = 0
			if np.isnan(upper):
				upper = 1
			f.writerow([pvals[j], lower, upper])