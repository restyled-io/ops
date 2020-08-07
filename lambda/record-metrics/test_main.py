import unittest
import main
import warnings

from dotenv import load_dotenv
from pathlib import Path
env_path = Path('.') / '.env.test'
load_dotenv(dotenv_path=env_path)


class TestMain(unittest.TestCase):
    def setUp(self):
        # https://github.com/boto/boto3/issues/454
        warnings.filterwarnings(
            "ignore", category=ResourceWarning, message="unclosed.*<ssl.SSLSocket.*>")

    def test_handler(self):
        result = main.handler(None, None)
        self.assertEqual(result['ok'], True)
        self.assertEqual(result['env'], 'test')


if __name__ == '__main__':
    unittest.main()
