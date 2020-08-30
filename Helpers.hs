{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Helpers where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (unsafeTExpCoerce)
import GHC.Exts
import GHC.ST

import MkSmallArray
import qualified MkSmallArrayOld as Old

-- create array using `smallArrayOf#` primop
new :: Int -> Code Q (Int -> Int)
new n = [|| \(I# i) ->
              case indexSmallArray# $$(smallArrayOf [0..n]) i of
                (# k #) -> k ||]

-- initialize new array to `undefined` and then unroll writing all slots
old :: Int -> Code Q (Int -> Int)
old n = [|| \(I# i) ->
              case indexSmallArray# $$(Old.smallArrayOf [0..n]) i of
                (# k #) -> k ||]

-- initialize new array to `undefined` and then write all slots in a loop
dynam :: Int -> Int -> Int
dynam (I# size) (I# i) = runST (ST $ \s0 ->
    let (# s1, marr #) = newSmallArray# size (undefined :: Int) s0 in
    let s2 = go marr 0# s1 in
    let (# s3, arr #) = unsafeFreezeSmallArray# marr s2 in
    let (# k #) = indexSmallArray# arr i in
    (# s3, k #))
  where
    go marr j s = case j ==# size of
      0# -> let s' = writeSmallArray# marr j (I# j) s in go marr (j +# 1#) s'
      _ -> s

listTE :: [Code Q a] -> Code Q [a]
listTE xs = unsafeCodeCoerce $ listE $ map unTypeCode xs
