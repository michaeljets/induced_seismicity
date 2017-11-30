def block_permute(ts, start, length): 
    """
    Each block is defined by the start point and length. 
    The possible starting points have to be from beginining to the length of the block 
    If starts in the middle and the n - l points aren't the multiple of length of the block, cut off the start
    and the end of the array

    Args:
        ts      (1D np.array)  : Time series(eg earthquake)
        start   (int)          : Start point of the block 
        length  (int)          : Length of the block 
        
    Returns:
        1D Array permuted block of time
    """
    assert np.any(ts[:length] == ts[start-1]), \
        "Starting point of the block is too far."
    assert length < len(ts),\
    "Block size too big"
    nb_blocks = np.floor((len(ts) - len(ts[: (start + length-1)]))/length +1)
    assert nb_blocks > 1, "block size is too big"
    nb_blocks = np.floor((len(ts) - len(ts[: (start + length-1)]))/length) +1
    end = int(start -1 + nb_blocks * length)

    shuffle_ts = ts[(start -1):end]
    shuffle_blocks = shuffle_ts.reshape(len(shuffle_ts)//length, length)
    np.random.shuffle(shuffle_blocks)
    return shuffle_blocks.ravel()