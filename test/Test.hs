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

viewSpec :: SpecWith ()
viewSpec = do
  it "replace tab" $
    replaceTabs "\tabcd\t efg\n" `shouldBe` " abcd  efg\n"
  it "buffer to content" $ do
    -- termSize (4, 5), render area (3, 5)
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
    cursor app' `shouldBe` Pos 1 4 -- cursor "stuck" at the edge
    offset app' `shouldBe` Pos 0 4 -- horizontal offset
