{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnboxedTuples #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module MkSmallArrayOld where

import GHC.Exts
import GHC.ST (ST(..), runST)
import Language.Haskell.TH.Syntax
	-- (Q, Exp(AppE, VarE, UnboxedTupE), Lift(lift), TExp(TExp))


data Box a = MkBox { unbox :: SmallArray# a }

{-# NOINLINE boom #-}
boom :: a
boom = error "boom!"

smallArrayOf :: Lift a => [a] -> Q (TExp (SmallArray# a))
smallArrayOf xs = TExp <$> (new =<< go 0 xs)
	where
		size = length xs
		new rest = pure
			(AppE
				(VarE 'unbox)
				(AppE
					(VarE 'runST)
					(AppE (ConE 'ST)
						(LamE
							[VarP $ mkName "s"]
							(AppE
								(LamE
									[UnboxedTupP [VarP $ mkName "s0", VarP $ mkName "marr"]]
									rest)
								(AppE
									(AppE
										(AppE
											(VarE 'newSmallArray#)
											(LitE (IntPrimL (fromIntegral size))))
										(VarE 'boom))
									(VarE $ mkName "s")))))))

		go ix [] = pure
			(LetE
				[ValD
					(UnboxedTupP [VarP $ mkName $ "s" <> show (ix + 1), VarP $ mkName "arr"])
					(NormalB
						(AppE
							(AppE
								(VarE 'unsafeFreezeSmallArray#)
								(VarE $ mkName "marr"))
							(VarE $ mkName $ "s" <> show ix)))
					[]]
				(UnboxedTupE
					[Just $ VarE $ mkName $ "s" <> show (ix + 1)
					,Just $ AppE (ConE 'MkBox) (VarE $ mkName "arr")]))
		go ix (x:xs) = do
			xs' <- go (ix + 1) xs
			x' <- lift x
			pure
				(LetE
					[ValD
						(VarP $ mkName $ "s" <> show (ix + 1))
						(NormalB
							(AppE
								(AppE
									(AppE
										(AppE
											(VarE 'writeSmallArray#)
											(VarE $ mkName "marr"))
										(LitE (IntPrimL (fromIntegral ix))))
									x')
								(VarE $ mkName $ "s" <> show ix)))

						[]]
					xs')

	-- [|| unbox (runST (ST $ \s0 ->
	-- 			let (# s1, marr #) = newSmallArray# 5# x s0 in
	-- 			$$( undefined
	-- 				--	 let s2 = writeSmallArray# marr 1# (x + 1) s1 in
	-- 				--	 let s3 = writeSmallArray# marr 2# (x + 2) s2 in
	-- 				--	 let s4 = writeSmallArray# marr 3# (x + 3) s3 in
	-- 				--	 let s5 = writeSmallArray# marr 4# (x + 4) s4 in
	-- 				--	 let (# s6, arr #) = unsafeFreezeSmallArray# marr s5 in
	-- 				--	 (# s6, MkBox arr #)
	-- 				))) ||]


--	do
--	 s0 <- newName "s"
--	 s1 <- newName "s"
--	 AppE (VarE 'runST) (AppE (ConE 'ST) LamE [VarP s0] (LetE [UnboxedTupP []]))

-- test :: Q Exp
-- test = [| let (# x, #) = (# 1 #) in x |]
-- runST (ST $ \s0 ->
--			 let (# s1, marr #) = newSmallArray# 5# x s0 in
--			 let s2 = writeSmallArray# marr 1# (x + 1) s1 in
--			 let s3 = writeSmallArray# marr 2# (x + 2) s2 in
--			 let s4 = writeSmallArray# marr 3# (x + 3) s3 in
--			 let s5 = writeSmallArray# marr 4# (x + 4) s4 in
--			 let (# s6, arr #) = unsafeFreezeSmallArray# marr s5 in
--			 (# s6, MkSmol arr #)) |]