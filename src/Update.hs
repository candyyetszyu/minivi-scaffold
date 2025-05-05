module Update where

import App
import System.Exit (exitSuccess)
import Util

-- | 'update' is the main update function for the program
update :: App -> Key -> IO App
update app k = case mode app of
  Normal -> pure $ handleNormal app k
  Insert -> pure $ handleInsert app k
  Command s -> handleCommand app s k
  Message m -> pure $ handleNormal (app {mode = Normal, dirty = FStatus}) k
  Search s -> pure $ handleSearch app s k

-- | 'moveCursor' moves cursor by 1 to the corresponding direction
moveCursor :: App -> Dir -> App
moveCursor app d =
  updateCursor app $ case d of
    DUp -> Pos (x - 1) y
    DDown -> Pos (x + 1) y
    DLeft -> Pos x (y - 1)
    DRight -> Pos x (y + 1)
  where
    Pos x y = contentPos app

-- | 'updateCursor'
-- given the new content cursor position, update cursor position and offset
updateCursor :: App -> Pos -> App
updateCursor app targetPos = 
  let 
    -- Get the current buffer dimensions (max valid position)
    maxRow = max 0 (length (buffer app) - 1)
    maxCol = if row targetPos <= maxRow && not (null (buffer app))
             then max 0 (length (buffer app !! row targetPos))
             else 0
            
    -- Bound the target position to valid limits
    boundedPos = Pos (max 0 (min maxRow (row targetPos))) (max 0 (min maxCol (col targetPos)))
    
    -- Calculate the terminal view size
    (termRows, termCols) = termSize app
    viewRows = max 0 (termRows - 2)  -- Account for status bar
    viewCols = max 0 termCols
    
    -- Calculate new cursor and offset
    newCursor = Pos newCursorRow newCursorCol
    newOffset = Pos newOffsetRow newOffsetCol
    
    -- Row calculations
    targetRow = row boundedPos
    curOffsetRow = row (offset app)
    
    -- Calculate new offset and cursor row
    newOffsetRow 
      | targetRow < curOffsetRow = targetRow -- Scroll up if target is above viewport
      | targetRow >= curOffsetRow + viewRows = targetRow - viewRows + 1 -- Scroll down if target is below viewport
      | otherwise = curOffsetRow -- Keep offset if target is within viewport
      
    newCursorRow = targetRow - newOffsetRow
    
    -- Column calculations
    targetCol = col boundedPos
    curOffsetCol = col (offset app)
    
    -- Calculate new offset and cursor column
    newOffsetCol
      | targetCol < curOffsetCol = targetCol -- Scroll left if target is left of viewport
      | targetCol >= curOffsetCol + viewCols = targetCol - viewCols + 1 -- Scroll right if target is right of viewport
      | otherwise = curOffsetCol -- Keep offset if target is within viewport
      
    newCursorCol = targetCol - newOffsetCol
    
    -- Update dirty flag
    newDirty 
      | dirty app == FFull = FFull
      | newOffsetRow /= curOffsetRow || newOffsetCol /= curOffsetCol = FFull
      | otherwise = FStatus
      
  in app { 
       cursor = newCursor, 
       offset = newOffset, 
       dirty = newDirty
     }

-- | 'handleNormal' handles update for normal mode
handleNormal :: App -> Key -> App
handleNormal app k = case k of
  KArrow d -> moveCursor app d
  KChar 'h' -> moveCursor app DLeft
  KChar 'j' -> moveCursor app DDown
  KChar 'k' -> moveCursor app DUp
  KChar 'l' -> moveCursor app DRight
  KChar 'w' -> moveToNextWord app
  KChar 'b' -> moveToPrevWord app
  KChar '0' -> moveToStartOfLine app
  KChar '$' -> moveToEndOfLine app
  KChar 'i' -> app {mode = Insert, dirty = FStatus}
  KChar ':' -> app {mode = Command "", dirty = FStatus}
  KChar 'u' -> undo app
  KChar 'U' -> redo app
  KChar '/' -> app {mode = Search "", dirty = FStatus}
  KChar 'n' -> findNextMatch app
  KChar 'N' -> findPrevMatch app
  _ -> app {dirty = FNone} -- ignore other key inputs in normal mode

-- | 'undo' restores the previous state from history
undo :: App -> App
undo app = case past app of
  [] -> app {mode = Message "Nothing to undo", dirty = FStatus}
  (h:hs) -> 
    let currentState = createHistoryEntry app
        restoredApp = restoreHistory app h
    in restoredApp {
        past = hs,
        future = take 4 (currentState : future restoredApp),
        mode = Normal,
        dirty = FFull,
        modified = not (null hs) || modified app -- Mark as modified unless we're back at original state
      }

-- | 'redo' restores the next state from future history
redo :: App -> App
redo app = case future app of
  [] -> app {mode = Message "Nothing to redo", dirty = FStatus}
  (h:hs) -> 
    let currentState = createHistoryEntry app
        restoredApp = restoreHistory app h
    in restoredApp {
        past = take 4 (currentState : past restoredApp),
        future = hs,
        mode = Normal,
        dirty = FFull,
        modified = True -- Always mark as modified when redoing
      }

-- | 'moveToNextWord' jumps to the start of next word
moveToNextWord :: App -> App
moveToNextWord app =
  let Pos row col = contentPos app
      buf = buffer app
      -- Check if row is valid
      rowValid = row >= 0 && row < length buf
      -- Get current line if row is valid
      currentLine = if rowValid then buf !! row else ""
      -- Find the next word start on current line
      remainingLine = drop col currentLine
  in 
    -- If we're already at the end of the line, go to the next line
    if col >= length currentLine && row < length buf - 1 
    then findNextWordNextLines app (row + 1)
    -- Otherwise try to find the next word on current line
    else 
      let nextWordInCurLine = findNextWordStart remainingLine col
      in case nextWordInCurLine of
          -- Found next word on current line
          Just newCol -> updateCursor app (Pos row newCol)
          -- No more words on current line, check next lines
          Nothing -> findNextWordNextLines app (row + 1)
  where
    -- Defines what characters are considered part of a word
    isWordChar c = isAlphaNum c || c == '_'
    isAlphaNum c = isAlpha c || isDigit c
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    isDigit c = c >= '0' && c <= '9'
    
    -- Find start of next word in a string, starting from base position
    findNextWordStart :: String -> Int -> Maybe Int
    findNextWordStart str basePos = 
      let 
        -- If we're currently on a word character, skip to the end of the word first
        (isOnWord, afterWord) = skipCurrentWord str
        -- After skipping the current word (if any), find the start of the next word
        skipNonWord = dropWhile (not . isWordChar) afterWord
      in 
        if null skipNonWord 
        then Nothing  -- No next word found
        else 
          -- Calculate the new position, accounting for the characters we skipped
          let charsSkipped = length str - length skipNonWord
          in Just (basePos + charsSkipped)
      
    -- Skip the current word if the cursor is on a word character
    skipCurrentWord :: String -> (Bool, String)
    skipCurrentWord [] = (False, [])
    skipCurrentWord (c:cs) 
      | isWordChar c = (True, dropWhile isWordChar (c:cs))
      | otherwise = (False, c:cs)
    
    -- Look for words in subsequent lines
    findNextWordNextLines :: App -> Int -> App
    findNextWordNextLines app r =
      let buf = buffer app
      in if r >= length buf
         -- No more lines, stay at end of file
         then app
         else
           let line = buf !! r
           in if null line
              -- Empty line, try next line
              then findNextWordNextLines app (r + 1)
              else
                -- Check if the line starts with a word character
                case dropWhile (not . isWordChar) line of
                  -- No word characters on this line, try next line
                  [] -> findNextWordNextLines app (r + 1)
                  -- Found word in this line, go to its position
                  rest -> updateCursor app (Pos r (length line - length rest))

-- | 'moveToPrevWord' jumps to the start of previous word
moveToPrevWord :: App -> App
moveToPrevWord app =
  let Pos row col = contentPos app
      buf = buffer app
      -- Check if row is valid
      rowValid = row >= 0 && row < length buf
      -- Get current line if row is valid
      currentLine = if rowValid then buf !! row else ""
      -- Get part of line before cursor
      beforeCursor = take col currentLine
  in if col > 0 && not (null beforeCursor)
     -- Try to find previous word on current line
     then case findPrevWordStart beforeCursor of
            Just newCol -> updateCursor app (Pos row newCol)
            Nothing -> findPrevWordPrevLines app (row - 1)
     -- At start of line, look in previous lines
     else findPrevWordPrevLines app (row - 1)
  where
    isSpace c = c == ' '
    
    -- Find start position of last word in a string
    findPrevWordStart :: String -> Maybe Int
    findPrevWordStart str =
      let -- Mark each character: is it a word start?
          charIsWordStart = zipWith (\prev cur -> isSpace prev && not (isSpace cur)) 
                           (' ' : init str) str
          -- Get positions where a word starts
          wordStarts = [i | (i, isStart) <- zip [0..] charIsWordStart, isStart]
      in if null wordStarts
         then Nothing
         else Just (last wordStarts)
    
    -- Look for words in previous lines
    findPrevWordPrevLines :: App -> Int -> App
    findPrevWordPrevLines app r =
      if r < 0
      -- No more lines, stay at start of file
      then app
      else
        let buf = buffer app
            line = if r < length buf then buf !! r else ""
        in case findPrevWordStart line of
             -- Found a word start in this line
             Just col -> updateCursor app (Pos r col)
             -- No word starts in this line, try previous line
             Nothing -> findPrevWordPrevLines app (r - 1)

-- | 'moveToStartOfLine' jumps to the first character of the line
moveToStartOfLine :: App -> App
moveToStartOfLine app =
  let Pos row _ = contentPos app
  in updateCursor app (Pos row 0)

-- | 'moveToEndOfLine' jumps to the last character of the line
moveToEndOfLine :: App -> App
moveToEndOfLine app =
  let Pos row _ = contentPos app
      buf = buffer app
      lineLength = if row < length buf then length (buf !! row) else 0
      -- Position at the last character (or 0 if empty line)
      lastCol = max 0 (lineLength - 1)
  in updateCursor app (Pos row lastCol)

-- 'handleInsert' handles update for insert mode
handleInsert :: App -> Key -> App
handleInsert app k = case k of
  KEsc -> app {mode = Normal, dirty = FStatus}
  KArrow d -> moveCursor app d
  KDel ->
    let (buf', pos') = bufDel (contentPos app) (buffer app)
        -- Save history before making the change
        appWithHistory = saveHistory app
     in updateCursor (appWithHistory {buffer = buf', dirty = FFull, modified = True}) pos'
  KChar c ->
    -- Save history before making the change
    let appWithHistory = saveHistory app
    -- Insert character and move cursor by 1 to the right
    in moveCursor (appWithHistory {buffer = bufIns c (contentPos app) (buffer app), dirty = FFull, modified = True}) DRight
  KRet ->
    let (buf', pos') = bufRet (contentPos app) (buffer app)
        -- Save history before making the change
        appWithHistory = saveHistory app
     in updateCursor (appWithHistory {buffer = buf', dirty = FFull, modified = True}) pos'

-- | 'bufIns' handles insertion of a single character
--
-- Given the character to be inserted and buffer, return the updated buffer
bufIns :: Char -> Pos -> [String] -> [String]
bufIns c (Pos i j) buf
  | i < 0 = buf                -- Out of bounds check for negative row
  | j < 0 = buf                -- Out of bounds check for negative column
  | i == length buf = buf ++ [[c]]  -- Append a new line with the character if at the end of buffer
  | i > length buf = buf       -- Out of bounds check for row beyond buffer + 1
  | otherwise = 
      let 
        currentLine = buf !! i
        -- Pad the current line with spaces if j is greater than the length of the line
        paddedLine = if j > length currentLine 
                    then currentLine ++ replicate (j - length currentLine) ' ' 
                    else currentLine
        -- Insert the character at position j
        (before, after) = splitAt j paddedLine
        newLine = before ++ [c] ++ after
        -- Update the buffer by replacing the line at position i
        updatedBuf = take i buf ++ [newLine] ++ drop (i+1) buf
      in updatedBuf

-- | 'bufDel' handles delete (backspace)
--
-- Given content cursor position and buffer, return the buffer and updated content cursor position
bufDel :: Pos -> [String] -> ([String], Pos)
bufDel (Pos i j) buf
  | i < 0 || i >= length buf = (buf, Pos i j)  -- Out of bounds check for row
  | j > 0 = 
      -- Case 1: Delete a character within the current line
      let currentLine = buf !! i
          newLine = take (j-1) currentLine ++ drop j currentLine
          newBuf = take i buf ++ [newLine] ++ drop (i+1) buf
      in (newBuf, Pos i (j-1))
  | i > 0 = 
      -- Case 2: At the beginning of a line, merge with previous line
      let prevLine = buf !! (i-1)
          currentLine = buf !! i
          prevLineLength = length prevLine
          newLine = prevLine ++ currentLine
          newBuf = take (i-1) buf ++ [newLine] ++ drop (i+1) buf
      in (newBuf, Pos (i-1) prevLineLength)
  | otherwise = (buf, Pos i j)  -- At the beginning of the file, do nothing

-- | 'bufRet' handles return
--
-- Given content cursor position and buffer, return the buffer and updated content cursor position
bufRet :: Pos -> [String] -> ([String], Pos)
bufRet (Pos i j) buf
  | i < 0 || i >= length buf = (buf, Pos i j)  -- Out of bounds check for row
  | otherwise = 
      let 
        currentLine = buf !! i
        -- Split the current line at cursor position
        (beforeCursor, afterCursor) = splitAt j currentLine
        -- Create new buffer with the split line
        newBuf = take i buf ++ [beforeCursor] ++ [afterCursor] ++ drop (i+1) buf
        -- New cursor position: at the beginning of the new line
        newPos = Pos (i+1) 0
      in (newBuf, newPos)

-- | 'handleCommand' handles update for command mode
handleCommand :: App -> String -> Key -> IO App
handleCommand app s k = case (s, k) of
  (_, KEsc) -> pure app {mode = Normal, dirty = FStatus}
  (s, KDel) -> pure $ case s of
    [] -> app {mode = Normal, dirty = FStatus}
    (_ : s') -> app {mode = Command s', dirty = FStatus}
  (s, KChar c) -> pure app {mode = Command (c : s), dirty = FStatus}
  (s, KRet) -> case parseCommand (reverse s) of
    Just ("q", _) -> 
      if modified app
        then pure app {mode = Message "Buffer modified, use ! to force quit", dirty = FStatus}
        else exitSuccess
    Just ("q!", _) -> exitSuccess
    Just ("w", Nothing) -> do
      -- Use the current file path if available
      if null (file app)
        then pure app {mode = Message "No file name specified", dirty = FStatus}
        else do
          writeFile (file app) (unlines $ buffer app)
          pure app {mode = Message ("File written to " ++ file app), dirty = FStatus, modified = False}
    Just ("w", Just filename) -> do
      -- Write to the specified filename
      writeFile filename (unlines $ buffer app)
      -- If we had no file name before, update the app's file path
      let updatedApp = if null (file app) 
                       then app {file = filename}
                       else app
      pure updatedApp {mode = Message ("File written to " ++ filename), dirty = FStatus, modified = False}
    _ -> pure app {mode = Message "Unrecognized command", dirty = FStatus}
  _ -> pure app {dirty = FNone}

-- | 'parseCommand' parses command strings like "w" or "w filename.txt"
-- Returns the command and optional argument
parseCommand :: String -> Maybe (String, Maybe String)
parseCommand [] = Nothing
parseCommand s = 
  let (cmd, rest) = break (== ' ') s
      arg = if null rest then Nothing else Just (dropWhile (== ' ') rest)
  in Just (cmd, arg)

-- | 'handleSearch' handles update for search mode
handleSearch :: App -> String -> Key -> App
handleSearch app s k = case k of
  KEsc -> app {mode = Normal, dirty = FStatus}
  KDel -> case s of
    [] -> app {mode = Normal, dirty = FStatus}
    (_ : s') -> app {mode = Search s', dirty = FStatus}
  KChar c -> app {mode = Search (c : s), dirty = FStatus}
  KRet -> 
    -- Execute the search when Enter is pressed
    let query = reverse s
    in if null query
       then app {mode = Normal, dirty = FStatus}
       else case performSearch query app Forward of
         Just pos -> 
           let updatedApp = updateCursor app pos
           in updatedApp {
               mode = Normal, 
               dirty = FFull, 
               searchQuery = query, 
               searchDirection = Forward
             }
         Nothing -> app {
             mode = Message ("Pattern not found: " ++ query), 
             dirty = FStatus,
             searchQuery = query
           }
  _ -> app {dirty = FNone}

-- | 'findNextMatch' finds the next occurrence of the search query
findNextMatch :: App -> App
findNextMatch app = 
  if null (searchQuery app)
  then app {mode = Message "No previous search", dirty = FStatus}
  else case performSearch (searchQuery app) app Forward of
    Just pos -> 
      let updatedApp = updateCursor app pos
      in updatedApp {dirty = FFull, searchDirection = Forward}
    Nothing -> app {
        mode = Message ("Pattern not found: " ++ searchQuery app), 
        dirty = FStatus
      }

-- | 'findPrevMatch' finds the previous occurrence of the search query
findPrevMatch :: App -> App
findPrevMatch app = 
  if null (searchQuery app)
  then app {mode = Message "No previous search", dirty = FStatus}
  else case performSearch (searchQuery app) app Backward of
    Just pos -> 
      let updatedApp = updateCursor app pos
      in updatedApp {dirty = FFull, searchDirection = Backward}
    Nothing -> app {
        mode = Message ("Pattern not found: " ++ searchQuery app), 
        dirty = FStatus
      }

-- | 'performSearch' searches for the query string in the buffer
-- Returns the position of the first match, or Nothing if not found
performSearch :: String -> App -> SearchDirection -> Maybe Pos
performSearch query app direction =
  if null query
  then Nothing
  else
    let 
      buf = buffer app
      currentPos = contentPos app
      
      -- Search functions for both directions
      findForward :: Int -> Int -> Maybe Pos
      findForward startRow startCol =
        let 
          -- Check current line from startCol
          currentLine = if startRow < length buf then buf !! startRow else ""
          restOfLine = drop startCol currentLine
          currentMatch = findSubstring query restOfLine
        in 
          case currentMatch of
            -- Found match on current line
            Just colOffset -> Just (Pos startRow (startCol + colOffset))
            -- Check remaining lines
            Nothing -> searchRemainingLines (startRow + 1)
      
      searchRemainingLines :: Int -> Maybe Pos
      searchRemainingLines rowIndex
        | rowIndex >= length buf = searchFromBeginning 0 -- Wrap around if at end
        | otherwise =
            let 
              line = buf !! rowIndex
              match = findSubstring query line
            in case match of
                 Just col -> Just (Pos rowIndex col)
                 Nothing -> searchRemainingLines (rowIndex + 1)
      
      -- Search from beginning up to current position (for wrapping)
      searchFromBeginning :: Int -> Maybe Pos
      searchFromBeginning rowIndex
        | rowIndex > row currentPos || (rowIndex == row currentPos && 0 >= col currentPos) = Nothing -- Searched entire buffer
        | otherwise =
            let 
              line = buf !! rowIndex
              searchPortion = if rowIndex == row currentPos
                             then take (col currentPos) line
                             else line
              match = findSubstring query searchPortion
            in case match of
                 Just col -> Just (Pos rowIndex col)
                 Nothing -> searchFromBeginning (rowIndex + 1)
      
      -- Search backward from current position
      findBackward :: Int -> Int -> Maybe Pos
      findBackward startRow startCol =
        let
          -- Check current line up to startCol
          currentLine = if startRow < length buf then buf !! startRow else ""
          beforeCursor = take startCol currentLine
          -- Find last occurrence in current line
          currentMatch = findLastSubstring query beforeCursor
        in
          case currentMatch of
            -- Found match on current line
            Just col -> Just (Pos startRow col)
            -- Check previous lines
            Nothing -> searchPreviousLines (startRow - 1)
      
      searchPreviousLines :: Int -> Maybe Pos
      searchPreviousLines rowIndex
        | rowIndex < 0 = searchFromEnd (length buf - 1) -- Wrap around if at beginning
        | otherwise =
            let
              line = buf !! rowIndex
              match = findLastSubstring query line
            in case match of
                 Just col -> Just (Pos rowIndex col)
                 Nothing -> searchPreviousLines (rowIndex - 1)
      
      -- Search from end down to current position (for wrapping)
      searchFromEnd :: Int -> Maybe Pos
      searchFromEnd rowIndex
        | rowIndex < row currentPos || (rowIndex == row currentPos && length (buf !! rowIndex) <= col currentPos) = Nothing -- Searched entire buffer
        | otherwise =
            let
              line = buf !! rowIndex
              searchPortion = if rowIndex == row currentPos
                             then drop (col currentPos) line
                             else line
              match = findLastSubstring query searchPortion
              -- Adjust column if searching in current row's portion
              adjustedCol = if rowIndex == row currentPos && match /= Nothing
                           then (+ col currentPos) <$> match
                           else match
            in case adjustedCol of
                 Just col -> Just (Pos rowIndex col)
                 Nothing -> searchFromEnd (rowIndex - 1)
                 
      -- Decide which search direction to use
      startSearch = case direction of
        Forward -> 
          if direction == searchDirection app && not (null (searchQuery app)) && searchQuery app == query
          then 
            -- Continue search from just after current position
            findForward (row currentPos) (col currentPos + 1)
          else
            -- Start new search from current position
            findForward (row currentPos) (col currentPos)
            
        Backward ->
          if direction == searchDirection app && not (null (searchQuery app)) && searchQuery app == query
          then 
            -- Continue search from just before current position  
            findBackward (row currentPos) (col currentPos)
          else
            -- Start new search from current position
            findBackward (row currentPos) (col currentPos + 1)
    in
      startSearch

-- | 'findSubstring' finds the first occurrence of a substring in a string
-- Returns the index of the first character of the match, or Nothing if not found
findSubstring :: String -> String -> Maybe Int
findSubstring needle haystack = 
  if null needle
  then Nothing
  else findSubstringHelper 0 haystack
  where
    findSubstringHelper :: Int -> String -> Maybe Int
    findSubstringHelper _ [] = Nothing
    findSubstringHelper index str
      | take (length needle) str == needle = Just index
      | otherwise = findSubstringHelper (index + 1) (drop 1 str)

-- | 'findLastSubstring' finds the last occurrence of a substring in a string
-- Returns the index of the first character of the match, or Nothing if not found
findLastSubstring :: String -> String -> Maybe Int
findLastSubstring needle haystack =
  if null needle
  then Nothing
  else findLastHelper 0 haystack Nothing
  where
    findLastHelper :: Int -> String -> Maybe Int -> Maybe Int
    findLastHelper _ [] lastFound = lastFound
    findLastHelper index str lastFound
      | take (length needle) str == needle = 
          findLastHelper (index + 1) (drop 1 str) (Just index)
      | otherwise = findLastHelper (index + 1) (drop 1 str) lastFound