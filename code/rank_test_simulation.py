import numpy as np
from scipy.stats import rankdata
import matplotlib.pyplot as plt
from simulate import largest_dot_normalized, simulate
import pickle

def main():
	print('Testing...')
	water1 = 1000-0.005*(np.arange(450)-150)**2+20*np.random.randn(450)
	eq1, p1 = generate_pvalues(water1, 0.0001, 0.0005, 0.047, 10)
	pickle.dump(water1, open('w.pickle', 'wb'))
	pickle.dump(eq1, open('eq.pickle', 'wb'))
	pickle.dump(p1, open('p.pickle', 'wb'))



def sim_eq(water, mu, beta, a, s1=1, s2=1):
	'''
	Simulate a time series of earthquake given water injection data,
	using McClure's model:
		y_j = Pois(exp(N(0,s1)) * (mu + beta*x_j) + a * y_(j-1) * exp(N(0,s2))).
	Returns an array with the same length as water. 
	'''
	eq=np.random.poisson(np.exp(np.random.normal(0,s1,1))*(mu+beta*water[0]))
	for i in range(len(water)-1):
		eq=np.append(eq,np.random.poisson(np.exp(np.random.normal(0,s1,1))*(mu+beta*water[i+1]) \
			+eq[i]*np.exp(np.random.normal(0,s2,1))*a))
	return eq
	

def get_pvalue(water, eq):
	'''
	Returns the p-value of the rank test.
	'''
	rank_w = rankdata(water)
	rank_e = rankdata(eq)
	og = largest_dot_normalized(rank_e, rank_w)
	s = simulate(rank_e, rank_w)
	return np.sum(s>og)/s.shape[0]


def generate_pvalues(water, mu, beta, a, n, plot=True):
	eq = list(map(lambda _: sim_eq(water, mu, beta, a), range(n)))
	p = list(map(lambda i: get_pvalue(water, eq[i]), range(n)))
	if plot:
		plt.hist(p,bins=20)
		plt.show()
	return eq, p


if __name__ == '__main__':
	main()