module Main where
import Parser
import OutputMisc
import LambdaCalc
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Attoparsec.Text hiding (number)


run :: Env -> IO()
run env = do
  putStrLn (show env)
  putStr "write> "
  input <- getLine
  case input of 
    "exit" -> putStrLn "bye"
    _ -> do
      let res = runeval env input
      putStrLn (show res)
      case res of
        Let a b -> do
           putStrLn (show res ++ "  yeah")
           run (Map.insert a b env)
        Var "Error" -> putStrLn "this is shit"
        _ -> do
          putStrLn (show res)
          run env


main :: IO ()
main = do
  putStrLn "Welcome To the most fucked up interpreter ever existed"
  run Map.empty
  