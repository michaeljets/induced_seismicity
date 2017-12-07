# get_eq.py
# Frank Mei, Lisa Jian, Michael Jetsupphasuk, My Dinh
# 04 December, 2017
# Earthquake generating functions.

"""
Context: 

Fill me in!

"""

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
