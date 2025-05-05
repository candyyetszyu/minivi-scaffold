module View where

import App
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Util

-- | 'render' is the main view function for the program
render :: App -> IO ()
render app = do
  putStr "\ESC[?25l" -- hide cursor
  case dirty app of
    FFull -> renderContent app *> renderStatus app
    FStatus -> renderStatus app
    FNone -> pure ()
  restoreCursorPos app
  putStr "\ESC[?25h" -- show cursor
  hFlush stdout

-- | 'replaceTabs' replace each tab with a space in a string
replaceTabs :: String -> String
replaceTabs = concatMap (\c -> if c == '\t' then " " else [c])

-- | 'bufferToContent' renders the buffer at the corret offset
--
-- It converts the buffer to a list of strings with $r-1$ lines, where each line has a length of $c$.
-- It should also replace all the tabs in the string to spaces.
bufferToContent :: App -> [String]
bufferToContent app = 
  let (Pos dy dx) = offset app       -- Get viewport offset (dy, dx) where dy is vertical, dx is horizontal
      (r, c) = termSize app          -- Get terminal size (r, c)
      buf = buffer app               -- Get the buffer
      
      offsetBuf = drop dy buf
      processedLines = map processLine offsetBuf

      viewportLines = r - 1 
      tildeLines = map (\_ -> '~' : replicate (c-1) ' ') [1..viewportLines]
      adjustedLines = take viewportLines (processedLines ++ tildeLines) 
  in adjustedLines
  where
    -- Helper function to process a single line
    processLine line = 
      let (_, c) = termSize app
          (Pos _ dx) = offset app
          -- Replace tabs with spaces using replaceTabs
          cleanLine = replaceTabs line
          -- Apply horizontal offset by dropping dx characters
          skippedLine = drop dx cleanLine
          -- Adjust line length to be exactly c (truncate or pad with spaces)
          adjustedLine = take c $ skippedLine ++ repeat ' '
      in adjustedLine

-- | Render the content buffer
renderContent :: App -> IO ()
renderContent app = setCursorPos 0 0 *> mapM_ putStrLn (bufferToContent app)

-- | Render the status bar
renderStatus :: App -> IO ()
renderStatus app = do
  let (r, c) = termSize app
      cp = cursor app
      rp = offset app
      f = file app
      buf = buffer app
      md = mode app
      status = case md of
        Normal -> printf "\"%s\" %dL" f (length buf)
        Insert -> "-- Insert --"
        Command s -> printf ":%s" (reverse (replaceTabs s))
        Search s -> printf "/%s" (reverse (replaceTabs s))
        Message m -> "[INFO] " <> m
  setCursorPos (r - 1) 0
  -- show some debug info (cursor and offset positions)
  let dbg = printf "(%d:%d);(%d:%d)" (row cp) (col cp) (row rp) (col rp)
  putStr $ take (c - length dbg) (status <> repeat ' ') <> dbg

-- | Restore console cursor position according to the cursor position stored in App
restoreCursorPos :: App -> IO ()
restoreCursorPos app = 
  let (r, c) = termSize app
  in case mode app of
       Command s -> setCursorPos (r - 1) (length s + 1)
       Search s -> setCursorPos (r - 1) (length s + 1)
       _ -> setCursorPos (row (cursor app)) (col (cursor app))
