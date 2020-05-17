#! /usr/bin/python3
import sys
import random

if len(sys.argv) < 5:
    print ("run with arguments: number to determine prime to use," + 
           " number of parties, the random a seed," +
           " and whether to test for reconstruct or verify")
    exit(0)


primes = [ 
  227,
  51407,
  3260360843,
  16855205958470386187, 
  336809720957688272968116945989114351147, 
  100932443431929982816198720791693327175050602902703518280128748060882240133359,
  12754709477010474220782419068422931704491832184660376415376491296628356815102803251176717402299823532992995848442800807715447900267393272594855354516567627,
  175150239024644694161690642159191781910910463333909797962399641113644974011784097900783034940881889044344867716330701716017301210554996353255650234107823397774674975724010953046911785617581821174422071047728368424235425612139451285532133680637314115431194717329268208256980317293902804291370699809368006311239]

prime = int(sys.argv[1])
parties = int(sys.argv[2])
seed = int(sys.argv[3])
reconstruct_test = sys.argv[4].lower() == "true"

random.seed(seed)

lower = 0
upper = 0
for p in primes:
    lower = upper
    upper = p
    if p >= prime:
        break

secret = random.randint(lower, upper//2)
print("initialize")
print(secret)
print(parties)
print(parties)
print(seed)

if reconstruct_test: 
    print ("reconstruct", str(list(range(1, parties+1))) )
else:
    print ("verify", parties // 2)

print("exit")
