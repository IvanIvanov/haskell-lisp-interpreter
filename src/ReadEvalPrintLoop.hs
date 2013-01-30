module Interpreter.ReadEvalPrintLoop
( readEvalPrintLoop
) where
import System.IO
import Interpreter.Core
import Interpreter.DataTypes
import Interpreter.Evaluator
import Interpreter.Parser

-- The welcome message to be output when the Read Eval Print Loop starts.
welcomeMessage :: String
welcomeMessage =
  "Welcome to the Read Eval Print Loop of this small Lisp interpreter!\n" ++
  " ___                  _       _     _             _   _            _    \n" ++
  "|_ _|_   ____ _ _ __ ( )___  | |   (_)___ _ __   | | | | __ _  ___| | __\n" ++
  " | |\\ \\ / / _` | '_ \\ \\/ __| | |   | / __| '_ \\  | |_| |/ _` |/ __| |/ /\n" ++
  " | | \\ V / (_| | | | | \\__ \\ | |___| \\__ \\ |_) | |  _  | (_| | (__|   < \n" ++
  "|___| \\_/ \\__,_|_| |_| |___/ |_____|_|___/ .__/  |_| |_|\\__,_|\\___|_|\\_\\\n" ++
  "                                         |_|                            \n" ++
  "To quit type quit on a line by itself.\n"

-- The Read Eval Print Loop itself.
loop :: Environment -> IO ()
loop env =
  do putStr "> "
     hFlush stdout
     line <- getLine
     if line == "quit"
       then return ()
       else do
         let parser = analyzeExpressionSequence . parseSequence . tokenize
             (newEnv, result) = (evalSequence env (parser line))
         putStrLn $ show result
         hFlush stdout
         loop newEnv

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do env <- loadCoreLibrary
                       putStr welcomeMessage
                       loop env

