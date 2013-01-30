#!/bin/bash

ghc --make -o bin/main  src/*.hs
rm src/*.hi
rm src/*.o

