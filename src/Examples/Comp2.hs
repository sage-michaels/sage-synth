{-# LANGUAGE TupleSections #-}

module Examples.Comp2 where

import Circuit
import Circuit.Builder
import Control.Monad

make :: IO [(Maybe String, Circuit)]
make = return [ (Just "comp2.dsl.acirc", comparison 4)]

comparison :: Int -> Circuit
comparison linputs = buildCircuit $ do
    xs             <- inputs linputs
    ys             <- inputs linputs
    nys            <- mapM circNot ys
    xsnys          <- zipWithM circMul xs nys --digit by digit x and (!y)
    nxsxory        <- zipWithM circXor xs ys -- digit by digit !(x XOR y) i.e. x and y are the same
    nxsxorys       <- mapM circNot nxsxory
    andedxors      <- chain circProd nxsxorys --list of previous XORs ANDed together
    let firstOr    = head xsnys
    let tlxsnys      = tail xsnys
    let frntandedxors  = init andedxors
    restOrs        <- zipWithM circMul tlxsnys frntandedxors
    let sofar      = firstOr : restOrs
    final          <- circSum sofar
    output final
  where
    chain f (x:xs)   = do
        rest <- helpchain f xs [x]
        return (x:rest)

    helpchain f [] _ = do 
        return []

    helpchain f (x:xs) ys = do
        z    <- f (x:ys)
        rest <- helpchain f xs (x:ys)
        return (z:rest)

