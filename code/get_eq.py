# get_eq.py
# Frank Mei, Lisa Jian, Michael Jetsupphasuk, My Dinh
# 04 December, 2017
# Earthquake generating functions.

"""
Context: 

Fill me in!

"""

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


def get_eq(water, mu=0.0000001, alpha=0.047, betas=[0.0000006, 0.0000006, 0.0000007, 0.0000009, 0.00001]):
    """
    Simulates earthquake data with a poisson process with a dependency on water at lag 0-4.
    Also includes a dependency on previous earthquake data.
    """

    eq = np.random.poisson(np.exp(np.random.randn(1)) * (mu + betas[4] * water[0]))
    for i in range(1, len(water)):
        if i < 4:
            new = np.random.poisson(np.exp(np.random.randn(1)) * (mu + betas[4] * water[i]) + \
                                    np.exp(np.random.randn(1)) * alpha * eq[i-1])
        else:
            new = np.random.poisson(np.exp(np.random.randn(1)) * (mu + \
                                                                 np.random.uniform(1) * betas[4] * water[i] + \
                                                                 np.random.uniform(1) * betas[3] * water[i-1] + \
                                                                 np.random.uniform(1) * betas[2] * water[i-2] + \
                                                                 np.random.uniform(1) * betas[1] * water[i-3] + \
                                                                 np.random.uniform(1) * betas[0] * water[i-4]) + \
                                    np.exp(np.random.randn(1)) * alpha * eq[i-1])

        eq = np.concatenate([eq, new])
    return eq
