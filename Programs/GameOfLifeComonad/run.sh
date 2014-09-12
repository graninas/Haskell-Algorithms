rm *.hi *.o GlossMetaLife
ghc -O2 -threaded -rtsopts GlossMetaLife.hs
./GlossMetaLife +RTS -N8 -RTS
