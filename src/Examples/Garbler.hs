{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Examples.Garbler where

import Circuit
import Circuit.Builder
import Circuit.Utils
import Examples.Goldreich

import Control.Monad
import Control.Monad.Trans (lift)
import Data.List.Split

import Debug.Trace

makeSizeTest :: IO [(Maybe String, Circuit)]
makeSizeTest = sequence
    [ (Just "size_test.acirc",) <$> sizeTest ]

--------------------------------------------------------------------------------
-- experimenting with writing a circuit for a garbled circuit scheme

testSwap :: Circuit
testSwap = buildCircuit $ do
    x <- inputs 2
    y <- inputs 2
    b <- constant 1
    [w,z] <- swap b x y
    outputs w

garblerTest :: IO Circuit
garblerTest = buildCircuitT $ do
    let k = 80 -- security parameter

    s <- inputs k

    g1 <- lift $ prg' k (6*k) (numBits k) xorAnd -- prg for generating wires
    g2 <- lift $ prg' k k     (numBits k) xorAnd -- prg for encrypting table entries
    g3 <- lift $ prg' k 2     (numBits k) xorAnd -- prg for generating permute bits

    -- generate wirelabels: inputs and output wires
    [x0,x1,y0,y1,z0,z1] <- chunksOf k <$> subcircuit g1 s
    let x = [x0,x1]
        y = [y0,y1]
        z = [z0,z1]

    unpermuted_gate <- forM (replicateM 2 [0,1] :: [[Int]]) $ \[i,j] -> do
        gate_x <- subcircuit g2 (x!!i)
        gate_y <- subcircuit g2 (y!!j)
        foldM1 (zipWithM circXor) [gate_x, gate_y, z !! b2i (i2b i && i2b j)]

    -- generate permute bits
    [b0,b1] <- subcircuit g3 s

    -- swap the ys based on b1
    p0 <- swap b1 (unpermuted_gate !! 0) (unpermuted_gate !! 1)
    p1 <- swap b1 (unpermuted_gate !! 2) (unpermuted_gate !! 3)
    let partially_permuted_gate = p0 ++ p1

    -- swap the xs based on b0
    p3 <- swap b0 (concat (take 2 partially_permuted_gate)) (concat (drop 2 partially_permuted_gate))
    let permuted_gate = concatMap (chunksOf k) p3

    outputs (concat permuted_gate)


sizeTest :: IO Circuit
sizeTest = buildCircuitT $ do
    let n = 80
    xs <- inputs n
    let g1 = prg_builder n (1000*n) (numBits n) xorAnd
    let g2 = prg_builder n n (numBits n) xorAnd
    ys <- chunksOf n <$> g1 xs
    z0 <- mapM g2 ys
    z1 <- zipWithM (zipWithM circMul) z0 ys
    z2 <- zipWithM (zipWithM circMul) z1 ys
    zs <- foldM1 (zipWithM circXor) z2
    outputs zs