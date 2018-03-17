# sim_functions.py
# Frank Mei, Lisa Jian, Michael Jetsupphasuk, My Dinh
# 04 December, 2017
# Functions to simulate lag-adjusted Spearman's rank correlation test for earthquakes

"""
Context: 

Fill me in!

"""

import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from scipy.stats import rankdata
from scipy.stats import pearsonr

##############################################################################

def largest_corr(rank1, rank2, lag=12, norm=np.inf):
    """
    Given two ranked vectors, take a window of length n-12,
    get a list of lag correlations by shifting the second rank vector,
    return the p-norm correlation in the list (default is maximum correlation).

    For special case of one list containing all same value (e.g. [1,1,1]), the
    correlation will equal one from a divide by 0 error (since sd is 0). For
    these cases, correlation is coerced to 0.

    Since `norm` can take an even integer which would treat positive and negative
    correlations the same, we take the max(0, correlation) since by assumption,
    we presume that injections do not suppress seismicity.
    """

    r1 = rank1[:-lag]
    corrs = list(map(lambda i: pearsonr(r1, rank2[i:(len(r1)+i)])[0], range(lag+1)))
<<<<<<< HEAD
    corrs = [0 if math.isnan(cor) else max(0, cor) for cor in corrs]
=======
    #corrs = [0 if math.isnan(cor) else cor for cor in corrs]
    corrs = [0 if math.isnan(cor) else max(0,cor) for cor in corrs]
>>>>>>> 47e7404abfb2ecacd80478a03e50f8e16559e9ee
    # return np.argmax(corrs) # which lag has the maximum correlation
    return np.linalg.norm(corrs, ord=norm)


def simulate(ranks1, ranks2, num_trials=10000, lag=12, norm=np.inf):
    """
    Given two ranked vectors, repeatedly permute ranks1 and get the 
    p-norm correlation for all lags specified. Return all simulated values.
    """

    # Used to simulate a single trial. The input is not used.
    def simulate_single_trial(_):
        np.random.shuffle(ranks1)
        return largest_corr(ranks1, ranks2, lag, norm)
    
    # Make a copy for ranks1 because np.random.shuffle is in place.
    ranks1 = ranks1.copy()

    return np.array(list(map(simulate_single_trial, range(num_trials))))


def simulate_by_block(ranks1, ranks2, bstart, bsize, lag=12, norm=np.inf, num_trials=5000):
    """
    Like `simulate` above except permutes by block specified by `bstart`
    and `bsize`.
    """

    def simulate_block_trial(_):
        r1, end = block_permute(ranks1, bstart, bsize)
        r2 = ranks2.copy()[bstart : end]
        return largest_corr(r1, r2, lag, norm)

    # Make a copy for ranks1 because block_permute is in place.
    ranks1 = ranks1.copy()

    return np.array(list(map(simulate_block_trial, range(num_trials))))


def p_value(dist, observed):
    """
    Given an empirical distribution `dist`, this function returns the
    probability of seeing `observed` or larger (i.e. this is one-sided).
    The empirical `dist`, for instance, would look like the return of
    the `simulate` function above.
    """

    return np.sum(dist >= observed) / float(len(dist))

def p_sd(R, p):
    """
    Given `R` replications and probability `p`, the standard deviation
    of a binomial(`R`, `p`) distribution is calculated. The purpose
    of this function is to provide an estimate of uncertainty for the
    p-value constructed from our estimated permutation test.
    """

    return (p*(1-p) / R) ** (0.5)


def corr_test(lst1, lst2, lag=12, norm=np.inf, bstart=0, bsize=6, plot=True, filename=None):
    """
    Combining simulation into a test that returns a p-value. Option
    to plot.
    """

    og = largest_corr(lst1, lst2, lag, norm)
    s = simulate_by_block(lst1, lst2, lag=lag, norm=norm, bstart=bstart, bsize=bsize)
    if plot:
        plt.hist(s, bins = 'auto')
        plt.axvline(x = og, color = 'red')
        plt.xlabel('{}-norm of correlations across {} lags'.format(norm, lag))
        plt.ylabel('counts')
        if filename:
            plt.savefig(filename)
        plt.show()
    pval = p_value(s, og)
    return pval, p_sd(len(s), pval)


def block_permute(ts, start, length): 
    """
    Each block is defined by the start point of to begin blocking and length of the block. 
    The possible starting points have to be from beginining to the length of the block. 
    If it starts in the middle and the n - l points aren't the multiple of length of the block,
    cut off the start and the end of the array

    Args:
        ts      (1D np.array) : Rank time series (e.g. earthquakes, water)
        start   (int)         : Start point of the block 
        length  (int)         : Length of the block 
        
    Returns:
        1D Array permuted block of time
        endpoint of cutoff vector
        
    """

    assert np.any(ts[:length] == ts[start]), \
        "Starting point of the block is too far."

    assert length < len(ts), \
    "Block size too big"

    nb_blocks = np.floor((len(ts) - len(ts[: (start + length)])) / length + 1)
    assert nb_blocks > 1, "block size is too big"
    end = int(start + nb_blocks*length)

    shuffle_ts = ts[start:end].copy()
    shuffle_blocks = shuffle_ts.reshape(len(shuffle_ts)//length, length)
    np.random.shuffle(shuffle_blocks)
    return shuffle_blocks.ravel(), end