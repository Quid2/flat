
-- Example of code equivalent to doctest
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}
module DocTest
    ( asPrint
    , test
    )
where
import qualified Data.Text                     as T
import           Test.Tasty
import           Test.Tasty.HUnit

class Print f where
    asPrint :: f -> IO String

instance Show a => Print (IO a) where
    asPrint io = io >>= return . show

instance {-# OVERLAPPABLE #-} Show a => Print a where
    asPrint a = return (show a)


-- test :: [String] -> IO String -> IO ()
-- test exp valIO = print $ testR exp (lines val)

-- testR exp val | exp == val = "OK"
--               | otherwise = unwords ["FAIL, expected",unlines exp,"but got",unlines val]

test loc exp valIO = do
    val <- lines <$> valIO
    return $ testCase loc (val @=? exp)
