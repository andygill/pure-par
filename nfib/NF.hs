-- To compile
-- ghc -threaded -rtsopts -eventlog -O2 NF.hs
-- To run on 4 processors
-- ./NF +RTS -N4

import GHC.Conc
import Criterion.Main

main = defaultMain $
     [ bench "sfib" (nf sfib 30)
     , bench "rfib" (nf rfib 30)
     ] ++ 
     [ bench ("dfib(" ++ show n ++ ")") (nf (dfib n) 30)
     | n <- [0..10]
     ]

--main = print (dfib 10 40)

sfib :: Integer -> Integer 
sfib n | n<2 = 1 
sfib n = sfib (n-1) + sfib (n-2) + 1

nfib :: Integer -> Integer 
nfib n | n<2 = 1 
nfib n = par nf (nf + nfib (n-2) + 1)
  where nf = nfib (n-1)

rfib :: Integer -> Integer 
rfib n | n < 2 = 1 
rfib n = nf1 `par` (nf2 `pseq` (nf1 + nf2 + 1)) 
      where nf1 = rfib (n-1) 
            nf2 = rfib (n-2) 

dfib :: Int -> Integer -> Integer 
dfib 0 n = sfib n
dfib d n = nf1 `par` (nf2 `pseq` (nf1 + nf2 + 1)) 
      where nf1 = dfib (d-1) (n-1) 
            nf2 = dfib (d-1) (n-2) 

