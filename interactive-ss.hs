import System.IO
import System.Random

-- Helper types
type Polynomial = [Integer]
type Share = Integer
type Generator = Integer

-- State for managing all necessary values
data State = State { 
              poly :: Polynomial, 
              shares :: [Share], 
              commitments :: [Integer],
              generator :: Integer,
              exitFlag :: Bool } deriving (Eq)

emptyState = createState [] [] [] 0

createState poly shares commitments generator = 
  (State {poly = poly, 
          shares = shares, 
          commitments = commitments, 
          generator = generator,
          exitFlag = False})

-- Text formatting
instance Show State where
  show s 
    | s == emptyState = "No secret initialized"
    | otherwise = 
        "\n" ++ statePolyString s ++ "\n" 
        ++ "g = " ++ show (generator s) ++ "\n"
        ++ stateSharesString s ++ "\n"
        ++ stateCommitmentsString s ++ "\n"

statePolyString :: State -> String
statePolyString s = 
  "p(x) = " ++ (prettyShow $ [0..] `zip` poly s)
    where prettyShow [(0, s)] = show s
          prettyShow [(1, s)] = show s ++ "x"
          prettyShow [(i, s)] = show s ++ "x^" ++ show i
          prettyShow ((0, s):ss) = show s ++ " + " ++ prettyShow ss
          prettyShow ((1, s):ss) = show s ++ "x + " ++ prettyShow ss
          prettyShow ((i, s):ss) = show s ++ "x^" ++ show i ++ " + " ++ prettyShow ss

            

stateSharesString :: State -> String
stateSharesString s = 
  "Shares:\n" ++ prettyShow ([1..] `zip` shares s)
    where prettyShow ((i, s):ss) = 
              "  p(" ++ show i ++ ") = " ++ show s ++ "\n" ++ prettyShow ss
          prettyShow [] = ""

stateCommitmentsString :: State -> String
stateCommitmentsString s = 
  "Commitment: " ++ show (commitments s)

-- Secret Sharing logic
calculateKeySpace :: Integer -> Integer
calculateKeySpace value = 
  let c = ceiling . logBase 2 . fromIntegral $ value in
  max 16 c

createPolynomial :: (RandomGen g) => Integer -> Int -> g -> Integer -> Polynomial
createPolynomial intersect degree g q = 
  intersect : (take degree . randomRs (0, q) $ g)

evaluatePoly :: Polynomial -> Integer -> Integer
evaluatePoly poly x = 
  sum . map (\(a,i) -> a * x^i) $ poly `zip` [0..]

createCommitments :: Polynomial -> Integer -> Integer -> [Integer]
createCommitments poly g q = map (\a -> g^a `mod` q) poly

createShares :: Polynomial -> Integer -> [Share]
createShares poly parties = 
  map (evaluatePoly poly) [1..parties]

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
  share_count <- prompt "Number of shares: "
  parties <- prompt "Number of parties: " 
  seed <- prompt "Random seed: "
  let (g, g') = split $ mkStdGen (fromInteger seed)
  let length = calculateKeySpace secret
  let poly = createPolynomial secret (fromInteger share_count) g length
  let shares = createShares poly parties
  let generator = 2
  let commitments = createCommitments poly generator length
  return $ createState poly shares commitments generator

printHelp = 
  putStrLn "\nCommands:\n\
            \  initialize: Setup a new secret sharing\n\
            \  reconstruct: Reconstructs the secret from set of given shares (reconstruct [i,...,t])\n\
            \  verify: Verfies a given share (verify i)\n\
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
handleQuery s@(State{}) "exit" = 
  return $ s {exitFlag = True}
handleQuery state "print" = print state >> return state

-- Parameterized queries
handleQuery state query 
  | command == "reconstruct" = return state
  | command == "verify" = return state
  | otherwise = do 
        putStrLn "Unknown command: Try \"help\"" 
        return state
    where (command, parameter) = break (==' ') query

-- Cute tiny entrypoint 
main = query emptyState
