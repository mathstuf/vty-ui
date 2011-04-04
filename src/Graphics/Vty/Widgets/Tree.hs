{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | TODO: scroll/cursor movement.

module Graphics.Vty.Widgets.Tree (
      Tree
    , treeData
    , TreeNode(..)
    , HasTreeAttr(..)
    , newTree
    , updateTreeWidgetFormat
    , updateTreeWidgetShowRoot
    , updateTreeWidgetTree
    ) where

import qualified Data.Tree as T
import Data.Tree.Path

import Graphics.Vty.Attributes
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Util

type TreeFormat = String

data Tree a = Tree
    { treeFormat :: TreeFormat
    , treeData :: T.Tree a
    , treeSelected :: TreePath
    , treeViewTop :: TreePath
    , treeShowRoot :: Bool
    , treeLinkAttr :: Attr
    }

instance Show a => Show (Tree a) where
    show = show . treeData

class HasTreeAttr a where
    setTreeLinkAttr :: a -> Attr -> IO ()

instance HasTreeAttr (Widget (Tree a)) where
    setTreeLinkAttr wRef a =
        updateWidgetState wRef $ \t ->
            t { treeLinkAttr = mergeAttr a (treeLinkAttr t) }

-- | Creates a new tree widget.
--
-- (@'newTree' showRoot format attr tree@) creates a new widget that uses
-- @format@ string to display extra information next to each node. If
-- @showRoot@ is 'False', then the root node is never shown. The attribute
-- @attr@ is used to color the links showing the hierarchy of nodes.
newTree :: TreeNode a => Bool -> TreeFormat -> Attr -> T.Tree a -> IO (Widget (Tree a))
newTree showRoot fmt attr tree = undefined

-- | Update the format string of a 'Tree'.
updateTreeWidgetFormat :: Widget (Tree a) -> TreeFormat -> IO ()
updateTreeWidgetFormat wRef fmt =
    updateWidgetState wRef $ \t ->
        t { treeFormat = fmt }

-- | Change whether the root of the tree is drawn.
updateTreeWidgetShowRoot :: Widget (Tree a) -> Bool -> IO ()
updateTreeWidgetShowRoot wRef showRoot =
    updateWidgetState wRef $ \t ->
        t { treeShowRoot = showRoot }

updateTreeWidgetTree :: Widget (Tree a) -> (T.Tree a -> T.Tree a) -> IO ()
updateTreeWidgetTree wRef f =
    updateWidgetState wRef $ \t ->
        t { treeData = f (treeData t) }

-- | Typeclass for a node in a tree widget.
class TreeNode a where
    -- | Whether the node is expanded or not.
    nodeExpanded :: a -> Bool
    nodeExpanded _ = True
    -- | Set the expanded flag. By default, nothing is done.
    nodeSetExpand :: a -> Bool -> a
    nodeSetExpand = const
    -- | A summary string for the node. This is used as the label for the tree.
    nodeSummary :: a -> String
    nodeSummary _ = ""
    -- | Formats a character. This is used to display extra information next
    -- to the tree.
    nodeFormatChar :: a -> Char -> String
    nodeFormatChar _ _ = ""
