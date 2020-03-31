import System.IO
import System.Random

type Polynomial = [Int]
type Share = Int

data State = State { 
              poly :: Polynomial, 
              shares :: [Share], 
              exitFlag :: Bool }

prompt :: String -> IO Int
prompt text = do 
  putStr text
  hFlush stdout
  readLn :: IO Int

promptS :: String -> IO String
promptS text = do 
  putStr text
  hFlush stdout
  getLine

-- createPolynomial :: (RandomGen g) => Int -> Int -> g -> [Int]
-- createPolynomial intersect degree g = 
--   intersect :: (take degree $ randoms g)

evaluatePoly :: [Int] -> Int -> Int
evaluatePoly poly x = 
  sum . map (\(a,i) -> a * x^i) $ poly `zip` [0..]

createShares :: [Int] -> Int -> [Int]
createShares poly parties = 
  map (evaluatePoly poly) [1..parties]
  

initialize :: IO ([Int], [Int])
initialize = do 
  secret <- prompt "Type the secret(integer): "
  share_count <- prompt "Number of shares: " 
  parties <- prompt "Number of parties: " 
  seed <- prompt "Random seed: "
  let poly = secret : take (share_count - 1) (randoms (mkStdGen seed))
  let shares = createShares poly parties
  return (poly, shares)

 
query :: State -> IO ()
query state = do 
  state' <- promptS "What to do? " >>= handleQuery state
  if exitFlag state' then return ()
  else query state'

handleQuery :: State -> String -> IO State
handleQuery state "help" = do
  putStrLn "This is help message" 
  return state

handleQuery state "exit" = 
  return (State (poly state) (shares state) True)
handleQuery state _ = do
  putStrLn "Unknown command"
  return state

main = do
  (poly, shares) <- initialize

  query (State poly shares False)

  putStrLn $ "Secret is: " ++ show (head poly)
  putStrLn $ show (length poly) ++ " shares"
  putStrLn $ show (length shares) ++ " parties"
  print poly
  print shares
