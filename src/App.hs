module App where

import Util

-- | Editor mode
data Mode
  = Normal -- normal mode
  | Insert -- insertion mode
  | Command String -- command mode (with command buffer)
  | Message String -- display message
  | Search String -- search mode (with search query)
  deriving (Show, Eq)

-- | Dirty flags
data Flag
  = FNone -- no refresh
  | FStatus -- refresh status bar only
  | FFull -- refresh all
  deriving (Show, Eq)

-- | History entry for undo/redo
data History = History
  { historyBuffer :: [String], -- text buffer at this point
    historyCursor :: Pos, -- cursor position
    historyOffset :: Pos -- viewport offset
  }
  deriving (Show)

-- | Search direction
data SearchDirection = Forward | Backward
  deriving (Show, Eq)

-- | The model (app state)
data App = App
  { mode :: Mode, -- current editor mode
    buffer :: [String], -- text buffer
    cursor :: Pos, -- cursor position
    offset :: Pos, -- view port offset
    dirty :: Flag, -- flag for refreshing terminal buffer
    modified :: Bool, -- buffer modified
    termSize :: (Int, Int), -- terminal size
    file :: FilePath, -- file path
    past :: [History], -- past states for undo
    future :: [History], -- future states for redo
    searchQuery :: String, -- current search query
    searchDirection :: SearchDirection -- current search direction
  }
  deriving (Show)

-- | 'newApp' initialize a new app
newApp :: FilePath -> String -> (Int, Int) -> App
newApp path buffer ts =
  App
    { mode = Normal,
      buffer = lines buffer,
      cursor = Pos 0 0,
      offset = Pos 0 0,
      dirty = FFull,
      modified = False,
      termSize = ts,
      file = path,
      past = [],
      future = [],
      searchQuery = "",
      searchDirection = Forward
    }

-- | 'contentPos' returns the content cursor position $(i, j)$
--
-- It should be cursor position + offset,
-- so you can either directly index the buffer or "off by one" (useful for editing)
contentPos :: App -> Pos
contentPos (App md buf (Pos x y) (Pos dx dy) u m ts f _ _ _ _) = Pos (x + dx) (y + dy)

-- | 'createHistoryEntry' creates a History entry from the current app state
createHistoryEntry :: App -> History
createHistoryEntry app = History
  { historyBuffer = buffer app,
    historyCursor = cursor app,
    historyOffset = offset app
  }

-- | 'saveHistory' adds the current state to the history
-- Keeps only the last 4 entries in history
saveHistory :: App -> App
saveHistory app = app
  { past = take 4 (createHistoryEntry app : past app),
    future = [] -- Clear future when new change is made
  }

-- | 'restoreHistory' restores app state from a History entry
restoreHistory :: App -> History -> App
restoreHistory app history = app
  { buffer = historyBuffer history,
    cursor = historyCursor history,
    offset = historyOffset history,
    dirty = FFull
  }