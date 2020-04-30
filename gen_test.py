import sys
import random

if len(sys.argv) < 3:
    print ("run with arguments for number of tests, and an initial random seed")
    exit(0)

random.seed(int(sys.argv[2]))

for _ in range(int(sys.argv[1])):
    secret = random.randint(0, 10000)
    share_num = random.randint(2,100)
    parties = share_num + random.randint(0, share_num)
    seed = random.randint(0, 100000)

    print("initialize")
    print(secret)
    print(share_num)
    print(parties)
    print(seed)
    for i in range(1, parties+1):
        print("verify", i)
print("exit")
