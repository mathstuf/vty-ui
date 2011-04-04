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

import Data.Char
import Data.Char.WCWidth
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Tree as T
import Data.Tree.Path

import Graphics.Vty.Attributes
import Graphics.Vty.DisplayRegion
import Graphics.Vty.Picture
import Graphics.Vty.Widgets.Core
import Graphics.Vty.Widgets.Skins
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
newTree showRoot fmt attr tree = do
    wRef <- newWidget $ \w ->
        w { state = Tree fmt tree initialPath initialPath showRoot attr
          , render_ = \this size ctx -> do
                t <- getState this
                let img = drawTreeWidget 0 (treeViewTop t) t size ctx
                    sz = (region_width size, region_height size)
                return $ crop sz $ pad sz $ img
          }
    return wRef
    where
        initialPath =
            if showRoot
                then pathRoot
                else pathFirstChild pathRoot

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

drawTreeWidget :: TreeNode a => Int -> TreePath -> Tree a -> DisplayRegion -> RenderContext -> Image
drawTreeWidget line path this size ctx
    | fromEnum (region_height size) == line = empty_image
    | otherwise                             =
        case pathLookup path tree of
            Just t  -> do
                let summaryAttr =
                        if path == treeSelected this
                            then focusAttr
                            else normalAttr
                    entry = string (normalAttr ctx) $ nodeFormat (T.rootLabel t) $ treeFormat this
                    summary = string (summaryAttr ctx) $ nodeSummary $ T.rootLabel t
                    fullLine = entry <|> treeLink <|> summary
                    nextPath =
                        if nodeExpanded (T.rootLabel t)
                            then pathFirstChild
                            else pathAfter
                fullLine <-> drawTreeWidget (line + 1) (nextPath path) this size ctx
            Nothing ->
                case pathParent' path of
                    Just ppath -> drawTreeWidget line (pathAfter ppath) this size ctx
                    Nothing    -> empty_image
        where
            tree = treeData this
            treeIndent ipath = fromMaybe "" $ do
                ppath <- pathParent' ipath
                _ <- pathParent' ppath
                let treeLine =
                        if isJust $ pathLookup (pathAfter ppath) tree
                            then skinVertical ctxSkin
                            else ' '
                Just $ treeIndent ppath ++ (treeLine:' ':' ':[])
            ctxSkin = skin ctx
            treeJoin =
                if isJust $ pathLookup (pathAfter path) tree
                    then skinIntersectionL
                    else skinCornerBL
            treeArrow = treeJoin ctxSkin:skinHorizontal ctxSkin:'>':[]
            summaryPrefix =
                let len = pathLength path
                in if len > 1
                    then treeIndent path ++ treeArrow
                    else ""
            treeLink =  string (treeLinkAttr this) summaryPrefix

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

nodeFormat :: TreeNode a => a -> String -> String
nodeFormat _    []           = ""
nodeFormat node ('%':'%':fs) = '%':nodeFormat node fs
nodeFormat node ('%':fs)     =
    case extractFormatSpecifier fs of
        Just (fm, ch, rest) ->
            let result = nodeFormatChar node ch
            in formatString fm result ++ nodeFormat node rest
        Nothing -> '%':nodeFormat node fs
nodeFormat node (f:fs)       = f:nodeFormat node fs

data Alignment = AlignLeft
               | AlignRight

data FormatModifiers = FM
    { fmAlign :: Alignment
    , fmLength :: Maybe Int
    }

extractFormatSpecifier :: String -> Maybe (FormatModifiers, Char, String)
extractFormatSpecifier fmt = do
    (align, palign) <- fmtAlign fmt
    (len, plen) <- fmtLen palign
    (ch, rest) <- fmtChar plen
    return (FM align len, ch, rest)
    where
        fmtAlign []       = Nothing
        fmtAlign ('+':fs) = Just (AlignRight, fs)
        fmtAlign ('-':fs) = Just (AlignLeft, fs)
        fmtAlign fs       = Just (AlignLeft, fs)
        fmtLen []         = Nothing
        fmtLen fss@(f:_)  | isNumber f = Just (Just $ read len, rest)
                          | otherwise  = Just (Nothing, fss)
            where
                (len, rest) = span isNumber fss
        fmtChar []        = Nothing
        fmtChar (f:fs)    | isAlpha f = Just (f, fs)
                          | otherwise = Nothing

formatString :: FormatModifiers -> String -> String
formatString fm str =
    case fmAlign fm of
        AlignLeft  -> base ++ padding
        AlignRight -> padding ++ base
    where
        len = wcswidth str
        (padding, base) =
            case fmLength fm of
                Just flen ->
                    if flen < len
                        then ("", take flen str)
                        else (replicate (flen - len) ' ', str)
                Nothing   -> ("", str)
