module Interpreter.Core
( coreEnvironment
, loadCoreLibrary
) where
import System.IO
import qualified Data.Map as Map
import Interpreter.DataTypes
import Interpreter.Evaluator
import Interpreter.Parser

-- Core built-in primitive functions.

-- Introspection functions.
isNull :: [Expression] -> Expression
isNull ((Null):[]) = (Boolean True)
isNull _ = (Boolean False)

isNumber :: [Expression] -> Expression
isNumber ((Number value):[]) = (Boolean True)
isNumber _ = (Boolean False)

isBoolean :: [Expression] -> Expression
isBoolean ((Boolean value):[]) = (Boolean True)
isBoolean _ = (Boolean False)

isVariable :: [Expression] -> Expression
isVariable ((Variable value):[]) = (Boolean True)
isVariable _ = (Boolean False)

isPair :: [Expression] -> Expression
isPair ((Pair _ _):[]) = (Boolean True)
isPair _ = (Boolean False)

-- Comparison functions.
numberEquals :: [Expression] -> Expression
numberEquals ((Number num1):(Number num2):[])
  | abs (num1 - num2) < 1e-6 = Boolean True
  | otherwise = Boolean False
numberEquals _ = (Boolean False)

booleanEquals :: [Expression] -> Expression
booleanEquals ((Boolean val1):(Boolean val2):[]) = (Boolean (val1 == val2))
booleanEquals _ = (Boolean False)

variableEquals :: [Expression] -> Expression
variableEquals ((Variable var1):(Variable var2):[]) = (Boolean (var1 == var2))
variableEquals _ = (Boolean False)

numberLessThan :: [Expression] -> Expression
numberLessThan ((Number num1):(Number num2):[]) = (Boolean (num1 < num2))
numberLessThan _ = Exception "Can't compare non-numbers."

numberGreaterThan :: [Expression] -> Expression
numberGreaterThan ((Number num1):(Number num2):[]) = (Boolean (num1 > num2))
numberGreaterThan _ = Exception "Can't compare non-numbers."

-- Arithmetic functions.
addNumbers :: [Expression] -> Expression
addNumbers ((Number num1):(Number num2):[]) = Number (num1 + num2)
addNumbers _ = Exception "Can only add two numbers"

subtractNumbers :: [Expression] -> Expression
subtractNumbers ((Number num1):(Number num2):[]) = Number (num1 - num2)
subtractNumbers _ = Exception "Can only subtract 2 numbers"

multiplyNumbers :: [Expression] -> Expression
multiplyNumbers ((Number num1):(Number num2):[]) = Number (num1 * num2)
multiplyNumbers _ = Exception "Can only add two numbers"

-- Logic functions.
notBoolean :: [Expression] -> Expression
notBoolean ((Boolean value):[]) = (Boolean (not value))
notBoolean _ = Exception "Can only negate a single boolean"

andBoolean :: [Expression] -> Expression
andBoolean ((Boolean value1):(Boolean value2):[]) =
  (Boolean (value1 && value2))
andBoolean _ = Exception "Can only negate a single boolean"

orBoolean :: [Expression] -> Expression
orBoolean ((Boolean value1):(Boolean value2):[]) =
  (Boolean (value1 || value2))
orBoolean _ = Exception "Can only negate a single boolean"

-- Pair manipulation functions.
createPair :: [Expression] -> Expression
createPair (first:second:[]) = Pair first second
createPair _ = Exception "A pair can only be created from 2 values"

pairFirst :: [Expression] -> Expression
pairFirst ((Pair first second):[]) = first
pairFirst _ = Exception "'car' is an operation defined on a pair."

pairSecond :: [Expression] -> Expression
pairSecond ((Pair first second):[]) = second
pairSecond _ = Exception "'cdr' is an operation defined on a pair."

createList :: [Expression] -> Expression
createList [] = Null
createList (e:es) = Pair e (createList es)

primitives = [
  ("null", Null),
  ("null?", PrimitiveProcedure isNull),
  ("number?", PrimitiveProcedure isNumber),
  ("boolean?", PrimitiveProcedure isBoolean),
  ("variable?", PrimitiveProcedure isVariable),
  ("pair?", PrimitiveProcedure isPair),
  ("number-equals?", PrimitiveProcedure numberEquals),
  ("boolean-equals?", PrimitiveProcedure booleanEquals),
  ("variable-equals?", PrimitiveProcedure variableEquals),
  ("list", PrimitiveProcedure createList),
  ("not", PrimitiveProcedure notBoolean),
  ("and", PrimitiveProcedure andBoolean),
  ("or", PrimitiveProcedure orBoolean),
  ("cons", PrimitiveProcedure createPair),
  ("car", PrimitiveProcedure pairFirst),
  ("cdr", PrimitiveProcedure pairSecond),
  ("+", PrimitiveProcedure addNumbers),
  ("-", PrimitiveProcedure subtractNumbers),
  ("*", PrimitiveProcedure multiplyNumbers),
  ("<", PrimitiveProcedure numberLessThan),
  (">", PrimitiveProcedure numberGreaterThan)]

-- The core environment containing all primitive functions.
coreEnvironment :: Environment 
coreEnvironment = Environment (Map.fromList primitives) EmptyEnvironment

-- The file containing the Lisp source code of the core library functions.
coreLibrary :: String
coreLibrary = "src/Core.lisp"

-- A helper function that extends the core environment with basic
-- Lisp library functions.
loadCoreLibrary :: IO Environment
loadCoreLibrary =
  do core <- readFile coreLibrary
     let parser = analyzeExpressionSequence . parseSequence . tokenize
         (environment, _) = (evalSequence coreEnvironment (parser core))
     return environment

