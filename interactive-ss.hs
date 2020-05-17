import System.IO
import System.Random
import Control.Monad
import Data.Maybe (fromJust)
import Data.List hiding (group)

import Debug.Trace (traceShow)

-- Helper types
type Polynomial = [Integer]
type Share = Integer
type Generator = Integer
type Group = Integer

-- Helper function
map2 = zipWith

powerMod :: Integer -> Integer -> Integer -> Integer
powerMod b e m = x
  where (_, _, x) = until cond step (b, e, 1)
        cond (_, e, _) = e <= 0
        isOdd = (/= 0) . (`mod` 2)
        step (b, e, x) = 
          ( b * b `mod` m,
            e `div` 2,
            if isOdd e then b * x `mod` m else x)

-- State for managing all necessary values
data State = State { 
              poly :: Polynomial, 
              shares :: [Share], 
              group :: Group,
              commitments :: [Integer],
              generator :: Generator,
              exitFlag :: Bool } deriving (Eq)

emptyState = createState [] [] 0 [] 0

createState :: Polynomial -> [Share] -> Group -> [Integer] -> Generator -> State
createState poly shares group commitments generator = 
  (State {poly = poly, 
          shares = shares, 
          group = group,
          commitments = commitments, 
          generator = generator,
          exitFlag = False})

-- Text formatting
instance Show State where
  show s
    | s == emptyState = "No secret initialized"
    | otherwise = 
        "\n" ++ polyToString (poly s) ++ "\n" 
        ++ "g = " ++ show (generator s) ++ "\n"
        ++ "|Zn*| = " ++ show (group s - 1) ++ "\n" 
        ++ stateSharesString s ++ "\n"
        ++ stateCommitmentsString s ++ "\n"

polyToString :: Polynomial -> String
polyToString poly = 
  "p(x) = " ++ (unwords . intersperse "+" $ map2 termShow [0..] poly)
    where termShow 0 term = show term
          termShow 1 term = show term ++ "x"
          termShow exponent term = show term ++ "x^" ++ show exponent

stateSharesString :: State -> String
stateSharesString s = 
  "Shares:\n" ++ unlines (map2 prettyShow (shares s) [1..])
    where prettyShow s i = 
              "  p(" ++ show i ++ ") = " ++ show s 

stateCommitmentsString :: State -> String
stateCommitmentsString s = 
  "Commitment: " ++ show (commitments s)

-- Secret Sharing logic
-- 2x 8 bits, 16 bits, etc. to 1024 bits safe primes
-- Calculating commitments take unreasonable amount of time for larger primes
safePrimes = [ 
  227,
  51407,
  3260360843,
  16855205958470386187, 
  336809720957688272968116945989114351147, 
  100932443431929982816198720791693327175050602902703518280128748060882240133359,
  12754709477010474220782419068422931704491832184660376415376491296628356815102803251176717402299823532992995848442800807715447900267393272594855354516567627,
  175150239024644694161690642159191781910910463333909797962399641113644974011784097900783034940881889044344867716330701716017301210554996353255650234107823397774674975724010953046911785617581821174422071047728368424235425612139451285532133680637314115431194717329268208256980317293902804291370699809368006311239]


selectGroup :: Integer -> Integer
selectGroup value = fromJust $ find (> value * 2) safePrimes 

selectGenerator :: (RandomGen g) => g -> Integer -> Integer
selectGenerator gen q = r^2 `mod` q
      where (r, _) = randomR (2, q-1) gen

createPolynomial :: (RandomGen g) => Integer -> Int -> g -> Group -> Polynomial 
createPolynomial intersect degree gen q = 
  intersect : take degree (randomRs (1, q-1) gen)

evaluatePoly :: Polynomial -> Integer -> Integer
evaluatePoly poly x = sum $ map2 (\a i -> a * x^i) poly [0..]

-- reconstruct helper functions

-- Used to simplify lagrange expression
-- Creates list of elements of the form [(x, [y, z]), (y, [x, z]), ...]
-- for all elements in list [x, y, z]
termElems :: [Integer] -> [(Integer, [Integer])]
termElems xjs = map (\xj -> (xj, filter (/= xj) xjs)) xjs

-- Each term in lagrange is calculated as f(xj) * lj(x)
-- lj(x) = product of xm / (xj - xm) over all values of xm
calcLagrangeTerm :: Group -> Integer -> Integer -> [Integer] -> Integer
calcLagrangeTerm q fxj xj xms = fxj * product ljx
  where ljx = map (\xm -> xm // (xm - xj)) xms
        (//) n m = n * calcInverse m q

calcInverse :: Integer -> Integer -> Integer
calcInverse a n = (x + n) `mod` n
  where (_,x,_) = eGCD a' n
        a' = if a < 0 then a + n else a

eGCD :: Integer -> Integer -> (Integer, Integer, Integer)
eGCD a b
  | r == 0 = (b, 0, 1)
  | otherwise = 
      let (d, x, y) = eGCD b r in 
      (d, y, x - y * q)
    where q = a `div` b
          r = a `mod` b

reconstruct :: [Share] -> [Int] -> Group -> Integer
reconstruct shares parties q = sum lagrangeTerms `mod` q
  where selectedShares = map (\p -> shares !! (p-1)) parties
        terms = termElems . map fromIntegral $ parties 
        lagrangeTerms = map2 (\fxj (xj, xms) -> calcLagrangeTerm q fxj xj xms) selectedShares terms

createCommitments :: Polynomial -> Generator -> Group -> [Integer]
createCommitments poly g q = map (\a -> powerMod g a q) poly

createShares :: Polynomial -> Integer -> Group -> [Share]
createShares poly parties q = 
  map (\i -> evaluatePoly poly i `mod` q) [1..parties]

calculateFeldmanProduct :: [Integer] -> Generator -> Integer -> Group -> Integer
calculateFeldmanProduct commitments g i q = product vals `mod` q
    where vals = map2 (\c p -> powerMod c p q) commitments commitPowers
          commitPowers = map (i^) $ [0..]

-- IO 
prompt :: String -> IO Integer
prompt text = do 
  putStr text
  hFlush stdout
  readLn :: IO Integer

promptS :: String -> IO String
promptS text = do 
  putStr text
  hFlush stdout
  getLine

initialize :: IO State
initialize = do 
  secret <- prompt "Type the secret(integer): "
  share_count <- prompt "Number of shares to reconstruct: "
  parties <- prompt "Number of parties: " 
  seed <- prompt "Random seed: "
  let (gen, gen') = split . mkStdGen . fromInteger $ seed
  let group = selectGroup secret
  let q = (group - 1) `div` 2
  let poly = createPolynomial secret (fromInteger share_count-1) gen q
  let shares = createShares poly parties q
  let generator = selectGenerator gen' group
  let commitments = createCommitments poly generator group
  return $ createState poly shares group commitments generator

printHelp = 
  putStrLn "\nCommands:\n\
            \  initialize: Setup a new secret sharing\n\
            \  reconstruct: Reconstructs the secret from set of given shares (reconstruct [i,...,t])\n\
            \  verify: Verfies a given share (verify i)\n\
            \  verify all: Verfies all parties\n\
            \  change_share: Changes the share of party i to value v (change_share i v)\n\
            \  print: Prints the polynomial, shares, commitments, and generator\n\
            \  help: Prints this help message\n\
            \  exit: Closes session\n"

query :: State -> IO ()
query state = do 
  state' <- promptS "What to do? " >>= handleQuery state
  if exitFlag state' then return ()
  else query state'

handleQuery :: State -> String -> IO State
handleQuery state "initialize" = initialize
handleQuery state "help" = printHelp >> return state
handleQuery state "exit" = 
  return $ state {exitFlag = True}
handleQuery state "print" = print state >> return state

-- Parameterized queries
handleQuery state query 
  | command == "reconstruct" = do 
      let parties = read parameter :: [Int]
      let p = reconstruct (shares state) parties ((group state - 1) `div` 2)
      putStrLn ("Reconstructed secret = " ++ show p) 
      return state
  | command == "verify" && parameter == " all" = do
      let parties = length $ shares state
      mapM (\i -> handleQuery state ("verify " ++ show i) ) [1..parties]
      return state
  | command == "verify" = do
      let party = read parameter :: Int
      let share = shares state !! (party - 1)
      let order = group state
      let g = generator state
      let x = calculateFeldmanProduct (commitments state) g (fromIntegral party) order
      let y = powerMod g share order
      putStrLn $ show (generator state) ++ "^P(" ++ show party ++ ") = " ++ show x ++ (if x == y then " true" else " false")
      return state
  | command == "change_share" = do
      let (p1, p2) = break (==' ') $ drop 1 parameter
      let party = read p1 :: Int
      let value = read p2 :: Integer
      let ss' = change (party-1) (shares state) value
      return  $ state {shares = ss'}
  | otherwise = do 
        putStrLn "Unknown command: Try \"help\"" 
        return state
    where (command, parameter) = break (==' ') query

change i list v = pf ++ (v:sf)
  where (pf, _:sf) = splitAt i list

-- Cute tiny entrypoint 
main = query emptyState
