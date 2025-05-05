module Util where

import Control.Exception (SomeException, catch)
import Control.Monad (guard)
import Foreign.C (CInt (..))
import System.IO
import Text.Printf (printf)

-- | Position
data Pos = Pos
  { row :: Int,
    col :: Int
  }
  deriving (Show, Eq)

-- | Direction for arrow keys
data Dir = DUp | DDown | DLeft | DRight deriving (Show, Eq)

-- | data type for Keys
data Key
  = KEsc -- esc key
  | KArrow Dir -- arrow key
  | KDel -- delete key
  | KRet -- return key
  | KChar Char -- other keys like a, A, \t, :
  deriving (Show, Eq)

foreign import ccall unsafe "term_row" c_term_row :: IO CInt

foreign import ccall unsafe "term_col" c_term_col :: IO CInt

-- 'getTermSize' returns terminal size (row, col)
--
-- Unfortunately this only works for unix-like system
getTermSize :: IO (Int, Int)
getTermSize = (`catch` \(e :: SomeException) -> pure (25, 80)) $ do
  r <- c_term_row
  c <- c_term_col
  guard $ r > 0 && c > 0
  pure (fromIntegral r, fromIntegral c)

-- | Clear screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J" *> hFlush stdout

-- | Move cursor to position $(x, y)$
setCursorPos :: Int -> Int -> IO ()
setCursorPos x y = putStr (printf "\ESC[%d;%dH" (x + 1) (y + 1)) *> hFlush stdout

-- | Read raw input
getKey :: IO Key
getKey = do
  c <- getChar
  case c of
    '\ESC' -> do
      r <- getEsc []
      pure $ case r of
        "[A" -> KArrow DUp
        "[B" -> KArrow DDown
        "[C" -> KArrow DRight
        "[D" -> KArrow DLeft
        "[3~" -> KDel
        _ -> KEsc -- we don't care about other keys
    '\n' -> pure KRet
    '\DEL' -> pure KDel -- delete and backspace are treated as backspace
    '\BS' -> pure KDel
    k -> pure (KChar k)
  where
    getEsc cs = do
      k <- hReady stdin
      if k then getChar >>= getEsc . (: cs) else pure (reverse cs)