-- |This module provides a scrollable view of another widget.
module Graphics.Vty.Widgets.Scroll
    ( HasScrollAttr(..)
    )
where

data RerenderEvent = RerenderEvent ()
data CursorMoveEvent = CursorMoveEvent (Maybe DisplayRegion)
data ScrollToEvent = SetViewToEvent (Word, Word) DisplayRegion

class HasScrollAttr a where
    setScrollBackgroundAttribute :: a -> Attr -> IO ()
    setScrollForegroundAttribute :: a -> Attr -> IO ()

class Scrollable a where
    renderAll :: a -> RenderContext -> IO Image
    calcCursorPosition :: a -> IO (Maybe DisplayRegion)
    onRerenderEvent :: a -> (RerenderEvent -> IO ()) -> IO ()
    onCursorMoveEvent :: a -> (CursorMoveEvent -> IO ()) -> IO ()
    onScrollToEvent :: a -> (ScrollToEvent -> IO ()) -> IO ()

data ScrollView a = ScrollView
    { scrollChild :: a
    , scrollImage :: Image
    , scrollCursor :: Maybe DisplayRegion
    , scrollBackAttr :: Attr
    , scrollForeAttr :: Attr
    , scrollShowHoriz :: Bool
    , scrollShowVert :: Bool
    }

scrolled :: (Scrollable a) => a -> IO ()
scrolled child = do
    wRef <- newWidget $ \w ->
        w {
          }
    return wRef
