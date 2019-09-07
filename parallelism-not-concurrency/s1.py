# From: https://old.reddit.com/r/Clojure/comments/cwgvvi/clojure_vs_blub_lang_parallelism/

import time
import timeit
from collections import deque
from concurrent.futures import ThreadPoolExecutor

my_data_log = deque()

def burner():
    start = time.time()
    i = 0
    while time.time() < start + 1:
        i = i + 1
    return i

def fetch_data(n):
    #time.sleep(1)
    burner()
    my_data_log.append(f"Fetched record: {n}")
    return n

def get_data():
    with ThreadPoolExecutor() as pool:
        pool.map(fetch_data, range(100))


print(timeit.timeit("get_data()", globals=globals(), number=1))

#print(burner())
