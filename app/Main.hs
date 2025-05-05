module Main where

import App (App, newApp)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch)
import System.Environment (getArgs)
import System.IO
import Update (update)
import Util
import View (render)

main :: IO ()
main = bracket prepare (const cleanup) mainLoop

-- setup file and raw mode
prepare :: IO App
prepare = do
  args <- getArgs
  ts <- getTermSize
  -- initialize app state
  app <- case args of
    [] -> do
      -- Start with an empty buffer when no file is specified
      let emptyBuffer = ""
      let defaultPath = "" -- Empty path indicates no file is open
      pure $ newApp defaultPath emptyBuffer ts
    path : _ -> do
      -- Try to read the file if it exists, otherwise start with empty buffer but remember the path
      buffer <- catch (readFile path) $ \(_ :: IOError) -> do
        putStrLn $ "New file: " ++ path
        return ""
      pure $ newApp path buffer ts
  -- setup raw mode
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  pure app

-- cleanup terminal after exit
cleanup :: IO ()
cleanup = setCursorPos 0 0 *> clearScreen

-- | Main loop that reads user input, update the model and render the view
mainLoop :: App -> IO ()
mainLoop app = do
  render app
  threadDelay 16660 -- 16.66ms delay (slightly larger than 60fps)
  key <- getKey
  update app key >>= mainLoop