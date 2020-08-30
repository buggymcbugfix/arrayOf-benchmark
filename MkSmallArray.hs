{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module MkSmallArray
  ( smallArrayOf
  , listToUnboxedTupE
  , expsToUnboxedTupE
  )
  where

import GHC.Settings.Constants (mAX_TUPLE_SIZE)
import GHC.Exts (SmallArray#, smallArrayOf#)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

smallArrayOf :: Lift a => [a] -> Code Q (SmallArray# a)
smallArrayOf xs = [|| smallArrayOf# $$(unsafeCodeCoerce $ listToUnboxedTupE xs) ||]

listToUnboxedTupE :: Lift a => [a] -> ExpQ
listToUnboxedTupE xs = do
  xsE <- traverse lift xs
  pure $ expsToUnboxedTupE xsE

expsToUnboxedTupE :: [Exp] -> Exp
expsToUnboxedTupE xs = UnboxedTupE $ map Just xsNested
  where
    xsNested
      | length xs <= mAX_TUPLE_SIZE
        = xs
      | otherwise
        = take (mAX_TUPLE_SIZE - 1) xs
        <> [expsToUnboxedTupE $ drop (mAX_TUPLE_SIZE - 1) xs]
