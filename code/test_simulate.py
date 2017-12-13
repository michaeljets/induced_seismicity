import unittest
import random
import sim_functions


class TestLargestCor(unittest.TestCase):
	def test_corr(self):
		"""
		test output when either array doesn't have all same values
		"""
		r1 = [1,2,3]
		r2 = [1,2,3]
		lag  = 1
		norm = 1
		expected= 2
		res = sim_functions.largest_corr(rank1 = r1, rank2 = r2, lag = lag,norm = norm)
		self.assertEqual(expected, res)

	def test_corr_same_rank(self):
		"""
		test correlation output when either vector has same values
		"""
		r1 = np.ones(12)
		r2 = np.arange(12)
		lag = 4
		norm = 2
		expected = 0
		res = sim_functions.largest_corrr(rank1 = r1, rank2 = r2, lag = lag, norm = norm)
		self.assertEqual(expected, res)

class TestSimulateBlock(unittest.TestCase):
	def test_block_permute_block_assertion(self):
		"""
		check exception for block size: block size has to be smaller than length and 
		need to make at least 2 block size 
		"""
		ts = np.arange(2,5)
		start = 1
		length = 4
		ts2 = np.arange(2, 7)
		self.assertRaises(AssertionError, sim_functions.block_permute, ts, start, length)
		self.assertRaises(AssertionError, sim_functions.block_permute, ts2, start, length)

	def test_block_permute_start_assertion(self):
		"""
		check exception for starting point 
		"""
		ts = np.arange(2,7)
		start = 3
		bsize = 2
		self.assertRaises(AssertionError, sim_functions.block_permute, ts, start, bsize)

	def test_block_permute(self):
		"""
		test output 
		"""
		ts = np.arange(2,7)
		start = 1
		bsize = 2
		res = sim_functions.block_permute(ts, start, bsize)
		expected_end = 5
		self.assertIsInstance(res, tuple)
		self.assertIsInstance(res[0], np.ndarray)
		self.assertIsInstance(res[1], int)
		self.assertEqual(res[1], expected_end)

	def test_simulate_block(self):
		r1 = np.arange(1, 10)
		r2 = np.arange(11,20)
		start = 2
		bsize = 3
		lag = 5
		norm =2 
		num_trials = 10
		res = sim_functions.simulate_by_block(r1, r2, start, bsize, lag = lag, norm = norm, num_trials = num_trials)
		expected = np.zeros(len(num_trials))
		res_bool = np.all(res == expected)
		self.assertTrue(res_bool)

class TestPvalue(unittest.TestCase):
		"""
		check if p value is between 0 and 1
		"""
		dist = np.arange(3,20)
		obs = 10
		res = p_value(dist, obs)
		expected_bool = 0 <= res <= 1
		self.assertTrue(expected_bool)


if __name__ == '__main__':
	unittest.main()