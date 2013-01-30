module Interpreter.Evaluator
( eval
, evalSequence
) where
import Interpreter.DataTypes

-- The "eval" function is the heart of the evaluator. It takes an environment
-- with variable bindings and a Lisp Expression to evaluate. The Expression is
-- evaluated returning a new environment, possibly containing new bindings, and
-- the Expression resulting from the evaluation process.
eval :: Environment -> Expression -> (Environment, Expression)
eval environment (Null) = (environment, Null)
eval environment (Boolean bool) = (environment, Boolean bool)
eval environment (Number number) = (environment, Number number)
eval environment (Variable variable) =
  (environment, lookupValue environment variable)
eval environment procedure@(Lambda parameters body) =
  (environment, procedure)

eval environment (Definition variable expression) =
  ((addBinding newEnvironment symbol value), value)
  where (newEnvironment, value) = eval environment expression
        (Variable symbol) = variable

eval environment (If predicate thenClause elseClause)
  | result == True = eval newEnvironment thenClause
  | otherwise = eval newEnvironment elseClause
  where (newEnvironment, (Boolean result)) = eval environment predicate

eval environment (Cond []) =
  (environment, (Exception "No case in cond special form matches."))
eval environment (Cond ((predicate, expression):xs))
  | result = eval newEnvironment expression
  | otherwise = eval newEnvironment (Cond xs)
  where (newEnvironment, (Boolean result)) = eval environment predicate

eval environment (Application operator operands) =
  let (firstEnvironment, op) = eval environment operator
      (finalEnvironment, arguments) = evalList firstEnvironment operands
      exception = firstException arguments
  in case op of
       (PrimitiveProcedure procedure) ->
         case exception of
           Just e -> (finalEnvironment, e)
           Nothing -> (finalEnvironment, procedure arguments)
       (Lambda params body) ->
         (finalEnvironment, result) 
           where env = (extendEnvironment finalEnvironment params arguments)
                 (_, result) = eval env body
       _ -> (firstEnvironment,
             Exception "Can't apply something which is not a procedure.")

eval environment _ = (environment, Exception "Can't evaluate - not supported")

evalList :: Environment -> [Expression] -> (Environment, [Expression])
evalList environment [] = (environment, [])
evalList environment (e:es) =
  let (firstEnvironment, resultFirst) = eval environment e
      (finalEnvironment, resultFinal) = evalList firstEnvironment es
  in (finalEnvironment, resultFirst : resultFinal)

firstException :: [Expression] -> Maybe Expression
firstException [] = Nothing
firstException (e:es) = case e of
                          (Exception string) -> Just e
                          _ -> firstException es

-- A helper function that evaluates a sequence of expressions one after
-- another passing an updated environment to each subsequent evaluation and
-- returns the value of the final Expression.
evalSequence :: Environment -> [Expression] -> (Environment, Expression)
evalSequence environment [] = (environment, Null)
evalSequence environment (e:[]) = eval environment e
evalSequence environment (e:es) = evalSequence env1 es
  where (env1, result) = eval environment e

