import unittest
from .test_ret import test_class


class TestClass(unittest.TestCase):
    def test_add_one(self):
        self.assertEqual(test_class().add_one(1), 2)


if __name__ == '__main__':
    unittest.main()
