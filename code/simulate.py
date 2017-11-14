import numpy as np

def dot_normalized(ranks1, ranks2, lag=0, use_zeros=False):
    """
    Shifts `ranks2` forward by `lag` number of entries.
    Removes the tail ends of the rank vectors that fall off
    after shifting. Then returns the dot product normalized
    by the number of entries that were used in the dot.
    For instance, suppose
         ranks1  = [1, 2, 3, 4]
         ranks2  = [4, 3, 2, 1]
         lag     = 2.
    After shifting and removing the fall off, we have
         ranks1' = [3, 4]
         ranks2' = [4, 3]
    The normalized dot is (3 * 4 + 4 * 3) / 2.

    Assumes that len(ranks1) == len(ranks2) and all ranks
    are non-negative.

    Args:
        ranks1    (1D np.array) : Ranks for first dataset
        ranks2    (1D np.array) : Ranks for second dataset
        lag       (int)         : Number of entries to shift
        use_zeros (bool)        : Use zeros in rank vectors
                                  for normalizing
    Returns:
        Normalized dot product after shifting by `lag`
    """
    assert len(ranks1) == len(ranks2), \
           "Rank vectors are not the same length"

    # Count the number of entries that would be zeroed out
    # in either vector
    num_zeros = 0
    if not use_zeros:
        num_zeros = np.sum((ranks1 * ranks2) == 0)

    # Pad rank vectors with zeros (from front w/ ranks1, 
    # from back w/ ranks2); equivalent to shifting
    padded_ranks1 = np.append(ranks1, np.zeros(lag))
    padded_ranks2 = np.insert(ranks2, 0, np.zeros(lag))

    # Normalization constant
    num_valid = float(len(ranks1) - lag - num_zeros)

    return np.dot(padded_ranks1, padded_ranks2) / num_valid

def largest_dot_normalized(ranks1, ranks2, min_lag=0, max_lag=12, use_zeros=False):
    """
    Returns the largest normalized dot product of the two rank vectors
    across all possible lags specified by `min_lag` and `max_lag`. 
    Inclusive of both `min_lag` and `max_lag`.
    """
    dots = [dot_normalized(ranks1, ranks2, lag, use_zeros) \
                for lag in range(min_lag, max_lag + 1)]
    return np.amax(dots)

def simulate(ranks1, ranks2, num_trials=10000, min_lag=0, max_lag=12, use_zeros=False):
    """
    Given two rank vectors, repeatedly permutes ranks and gets 
    the largest normalized dot product. Return all simulated values.
    Like `dot_normalized()`, this assumes we are shifting `rank2`
    forward.
    """
    # Used to simulate a single trial. The input is not used
    def simulate_single_trial(_):
        np.random.shuffle(ranks1)
        return largest_dot_normalized(ranks1, ranks2, min_lag, max_lag, use_zeros)
    
    # Make a copy for ranks1 because np.random.shuffle is in place
    ranks1 = ranks1.copy()

    return np.array(list(map(simulate_single_trial, range(num_trials))))
