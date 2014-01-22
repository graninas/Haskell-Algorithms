del *.hi
del *.o
del FailureProbability.exe

ghc -O2 FailureProbability.hs -rtsopts -threaded
