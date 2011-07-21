import qualified Data.Tree as T

import Graphics.Vty.Attributes
import Graphics.Vty.DisplayRegion
import Graphics.Vty.LLInput
import Graphics.Vty.Widgets.Borders
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.EventLoop
import Graphics.Vty.Widgets.Tree

newtype Node = Node String deriving (Show)
instance TreeNode Node where
    nodeSummary (Node s) = s

main :: IO ()
main = do
    let node = Node "Hello world!"
    let nodeCC1 = T.Node (Node "foo") []
    let nodeCC2 = T.Node (Node $ concat $ replicate 1 "something really long that ") []
    let nodeC1 = T.Node (Node "baz") [nodeCC1, nodeCC2]
    let nodeC2 = T.Node (Node "bar") []
    let trie = T.Node node [nodeC1,nodeC2]
    t_ <- newTree False "" def_attr trie
    t1 <- bordered t_
    t <- bordered t1

    img <- render t (DisplayRegion 40 10) defaultContext

    fg <- newFocusGroup
    addToFocusGroup fg t

    c <- newCollection
    coll <- addToCollection c t fg

    t_ `onKeyPressed` \this key mods -> do
        if null mods && key == KASCII 'q'
            then error "Done!"
            else return ()
        if null mods && key == KASCII 't'
            then updateTreeWidgetShowRoot this True
            else return ()
        if null mods && key == KASCII 'T'
            then updateTreeWidgetShowRoot this False
            else return ()
        if null mods && key == KASCII 'h'
            then updateTreeWidgetMove this Parent
            else return ()
        if null mods && key == KASCII 'j'
            then updateTreeWidgetMove this Next
            else return ()
        if null mods && key == KASCII 'k'
            then updateTreeWidgetMove this Previous
            else return ()
        if null mods && key == KASCII 'l'
            then updateTreeWidgetMove this Child
            else return ()
        if null mods && key == KASCII 'J'
            then updateTreeWidgetScroll this ScrollDown
            else return ()
        if null mods && key == KASCII 'K'
            then updateTreeWidgetScroll this ScrollUp
            else return ()
        return True

    runUi c defaultContext
