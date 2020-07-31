
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main

import Helpers

import MkSmallArray
import MkSmallArrayOld
import GHC.Exts
-- -- UNIT TEST
-- main = do
--   print $ map $$(new 5) [0..4]
--   print $ map $$(old 5) [0..4]
--   print $ map (dynam 5) [0..4]

main :: IO ()
main = defaultMain
  [ bgroup
      "foo"
      $$(let ns = [3] in -- change iterations here
          listTE $ concatMap (\(x,y,z) -> [x,y,z]) $ zip3
            (map (\n -> [|| bench ("new/" <> show n) $ nf @Int $$(new n) (n - 1) ||]) ns)
            (map (\n -> [|| bench ("old/" <> show n) $ nf @Int $$(old n) (n - 1) ||]) ns)
            (map (\n -> [|| bench ("dyn/" <> show n) $ nf @Int (dynam n) (n - 1) ||]) ns)
        )
  ]
