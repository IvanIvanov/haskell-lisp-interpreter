ghc --make -o bin\main ^
  src\Main.hs ^
  src\ReadEvalPrintLoop.hs ^
  src\Core.hs ^
  src\DataTypes.hs ^
  src\Evaluator.hs ^
  src\Parser.hs

del src\Main.o ^
    src\Main.hi ^
    src\ReadEvalPrintLoop.o ^
    src\ReadEvalPrintLoop.hi ^
    src\Core.o ^
    src\Core.hi ^
    src\DataTypes.o ^
    src\DataTypes.hi ^
    src\Evaluator.o ^
    src\Evaluator.hi ^
    src\Parser.o ^
    src\Parser.hi

