module Main where
import Parser
import OutputMisc
import LambdaCalc
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Attoparsec.Text hiding (number)
import Check

run :: Env -> TEnv -> IO()
run venv tenv = do
  -- putStrLn (show venv)
  -- putStrLn (show tenv)
  putStr "write> "
  input <- getLine
  case input of 
    "exit" -> putStrLn "bye"
    ('!':xs) -> do
      case (Map.lookup xs tenv) of
        Just t ->  putStrLn ((show t))
        Nothing -> putStrLn ((show xs) ++ " doesn't exist")
      run venv tenv

    _ -> do
      -- putStrLn (show input)
      let tp = typeChecker input tenv 
      let res = runeval venv input
      
      case tp of 
        Right exptype -> do
          case res of
            Let a b -> do
              putStrLn "wow how did you just write a working expression !!"
              run (Map.insert a b venv) (Map.insert a exptype tenv)
            Var "Error" -> do
              putStrLn "this is shit"
              run venv tenv
            _ -> do
              putStrLn (show res)
              run venv tenv

        Left err -> do
          putStrLn (show err)
          run venv tenv
        
      
      
   --       


main :: IO ()
main = do
  putStrLn "Welcome To the most fucked up interpreter ever existed"
  run Map.empty Map.empty
  