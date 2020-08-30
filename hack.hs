import Data.List

go = writeFile "grr.hs" ("[ " <> intercalate "\n, " (map hackSmallArrayOf [1..500]) <> "\n]")

hackSmallArrayOf n = "[|| bench (\"arrayOf/" <> show n <> "\") $ nf @Int (\\i@(I# i#) -> case indexSmallArray# (smallArrayOf# " <> hackUnboxedTuple ("i" : map show [1..n]) <> ") i# of (# k #) -> k) " <> show (n - 1) <> "||]"

mkUnboxedTuple xs = "(# "  <> intercalate ", " xs <> " #)"

hackUnboxedTuple xs = mkUnboxedTuple
  (if length xs <= 62 then xs else take 61 xs <> [hackUnboxedTuple $ drop 61 xs])
