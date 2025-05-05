import App
import Test.Hspec
import Update
import Util
import View

main :: IO ()
main = hspec $ do
  insertSpec
  cursorSpec
  viewSpec
  searchSpec
  navigationSpec
  historySpec

viewSpec :: SpecWith ()
viewSpec = do
  it "replace tab" $
    replaceTabs "\tabcd\t efg\n" `shouldBe` " abcd  efg\n"
  it "buffer to content" $ do
    let app = newApp "" (unlines ["abcdefg", "hijk", "lmn", "op"]) (4, 5)
    bufferToContent app `shouldBe` ["abcde", "hijk ", "lmn  "]
    let app' = app {offset = Pos 2 1}
    bufferToContent app' `shouldBe` ["mn   ", "p    ", "~    "]

insertSpec :: SpecWith ()
insertSpec = do
  let buffer = ["hello", "world"]
  it "insert buffer" $ do
    bufIns 'l' (Pos 0 2) buffer `shouldBe` ["helllo", "world"]
    bufIns '!' (Pos 1 5) buffer `shouldBe` ["hello", "world!"]
    bufIns '.' (Pos 2 0) buffer `shouldBe` (buffer <> ["."])
  it "delete buffer" $ do
    bufDel (Pos 0 0) buffer `shouldBe` (buffer, Pos 0 0)
    bufDel (Pos 0 1) buffer `shouldBe` (["ello", "world"], Pos 0 0)
    bufDel (Pos 1 0) buffer `shouldBe` (["helloworld"], Pos 0 5)
  it "return buffer" $ do
    bufRet (Pos 0 0) buffer `shouldBe` ("" : buffer, Pos 1 0)
    bufRet (Pos 1 2) buffer `shouldBe` (["hello", "wo", "rld"], Pos 2 0)
    bufRet (Pos 1 5) buffer `shouldBe` (buffer <> [""], Pos 2 0)

cursorSpec :: SpecWith ()
cursorSpec = do
  let app = (newApp "" (unlines ["hello", "world!!!", "bye"]) (4, 5)) {cursor = Pos 1 4}
  it "move cursor" $ do
    contentPos (moveCursor app DLeft) `shouldBe` Pos 1 3
    contentPos (moveCursor app DRight) `shouldBe` Pos 1 5
    contentPos (moveCursor app DUp) `shouldBe` Pos 0 4
    contentPos (moveCursor app DDown) `shouldBe` Pos 2 3
  it "update cursor" $ do
    let app' = updateCursor app (Pos 1 8)
    cursor app' `shouldBe` Pos 1 4
    offset app' `shouldBe` Pos 0 4

searchSpec :: SpecWith ()
searchSpec = do
  let buffer = ["this is a test", "find me in this line", "also find me here"]
      app = (newApp "" (unlines buffer) (10, 30)) {cursor = Pos 0 0}
  
  it "forward search" $ do
    let result = performSearch "find" app Forward
    result `shouldBe` Just (Pos 1 0)
    
  it "search from middle of buffer" $ do
    let app' = app {cursor = Pos 1 5}
        result = performSearch "find" app' Forward
    result `shouldBe` Just (Pos 2 5)
    
  it "backward search" $ do
    let app' = app {cursor = Pos 2 10}
        result = performSearch "find" app' Backward
    result `shouldBe` Just (Pos 2 5)
    
  it "search not found" $ do
    let result = performSearch "nonexistent" app Forward
    result `shouldBe` Nothing

navigationSpec :: SpecWith ()
navigationSpec = do
  let buffer = ["hello world", "word1 word2 word3", "start_end"]
      app = (newApp "" (unlines buffer) (10, 30)) {cursor = Pos 1 6}
      
  it "move to next word" $ do
    contentPos (moveToNextWord app) `shouldBe` Pos 1 12
  
  it "move to previous word" $ do
    contentPos (moveToPrevWord app) `shouldBe` Pos 1 0
    
  it "move to start of line" $ do
    contentPos (moveToStartOfLine app) `shouldBe` Pos 1 0
    
  it "move to end of line" $ do
    contentPos (moveToEndOfLine app) `shouldBe` Pos 1 16

historySpec :: SpecWith ()
historySpec = do
  let initialBuffer = ["initial text"]
      app = newApp "" (unlines initialBuffer) (10, 30)
      
  it "saveHistory preserves state" $ do
    let app' = saveHistory app
    length (past app') `shouldBe` 1
    historyBuffer (head (past app')) `shouldBe` initialBuffer
    
  it "handle undo with no history" $ do
    let result = undo app
    mode result `shouldBe` Message "Nothing to undo"
    
  it "handle redo with no future history" $ do
    let result = redo app
    mode result `shouldBe` Message "Nothing to redo"
    
  it "undo/redo operations" $ do
    let initialBuffer = ["initial text"]
        app = newApp "" (unlines initialBuffer) (10, 30)
        app1 = saveHistory app
        app2 = app1 {buffer = ["modified text"], modified = True}
        app3 = saveHistory app2
        app4 = undo app3
        _ = buffer app4 `shouldBe` initialBuffer
        app5 = redo app4
    
    buffer app5 `shouldBe` ["modified text"]
