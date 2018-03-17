# get_eq.py
# Frank Mei, Lisa Jian, Michael Jetsupphasuk, My Dinh
# 04 December, 2017
# Earthquake generating functions.

"""
Context: 

Fill me in!

"""
import numpy as np

def get_eq_simple(water, mu=0.0000001, beta=0.00001):
    """
    Simulates earthquake data with a poisson process with a dependency on water at lag 0.
    """

    eq = np.array([np.random.poisson(np.exp(np.random.randn(1)) * (mu + beta * water[i])) 
                   for i in range(len(water))])
    return eq


def get_eq_timelag(water, mu=0.0000001, beta=0.00001, alpha=0.047):
    """
    Simulates earthquake data with a poisson process with a dependency on water at lag 0.
    Also includes a dependency on previous earthquake data.
    """

    eq = np.random.poisson(np.exp(np.random.randn(1)) * (mu + beta * water[0]))
    for i in range(1, len(water)):
        new = np.random.poisson(np.exp(np.random.randn(1)) * (mu + beta * water[i]) \
                                + np.exp(np.random.randn(1)) * alpha * eq[i-1])
        eq = np.concatenate([eq, new])
    return eq


def get_eq(water, betas, mu, alpha=.01):
    """
    Simulates earthquake data with a poisson process with a dependency on water and past 
    seismicity. See paper for more details. 

    Note: originally mu = .0000001, alpha = .047, betas=[0.0000006, 0.0000006, 0.0000007, 0.0000009, 0.00001]

    Note2: in McClure, mu varies wildly; small mu is associated with low p-values, vice versa
    """

    betas = np.array(betas)
    water = np.array(water)

    year_var = np.exp(np.random.randn(1))
    mu_cont = np.array(year_var * mu)
    water_cont = np.array(year_var * betas[-1] * water[0])
    clust_cont = np.array([0])

    eq = np.array([np.random.poisson(mu_cont[0] + water_cont[0])])

    for i in range(1, len(water)):

        if i < (len(betas)-1):

            year_var = np.exp(np.random.randn(1))
            year_var2 = np.exp(np.random.randn(1))

            mu_cont = np.append(mu_cont, year_var * mu)
            water_cont = np.append(water_cont, year_var * betas[-1] * water[i])
            clust_cont = np.append(clust_cont, year_var2 * alpha * eq[i-1])

            new = np.array([np.random.poisson(mu_cont[i] + water_cont[i] + clust_cont[i])])

        else:

            year_var = np.exp(np.random.randn(1))
            year_var2 = np.exp(np.random.randn(1))

            mu_cont = np.append(mu_cont, year_var * mu)
            water_cont = np.append(water_cont, year_var * np.sum(betas * water[(i-len(betas)+1):i+1]))
            clust_cont = np.append(clust_cont, year_var2 * alpha * eq[i-1])

            new = np.array([np.random.poisson(mu_cont[i] + water_cont[i] + clust_cont[i])])

        eq = np.concatenate([eq, new])

    # if there is no seismicity at any time, place one randomly
    if all(eq == 0):
        ind = np.random.choice(range(len(eq)))
        eq[ind] = 1

    # # print overall lambda range
    # print("Mean mu contribution: ", np.mean(mu_cont))
    # print("Mean water contribution: ", np.mean(water_cont))
    # print("Mean clust contribution: ", np.mean(clust_cont))
    # print("Mean total contribution: ", np.mean(np.sum(np.array([mu_cont, water_cont, clust_cont]), 0)))

    return eq
