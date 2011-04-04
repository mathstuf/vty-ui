{-# LANGUAGE ForeignFunctionInterface #-}

-- Have our own function here while wcwidth doesn't compile.
-- https://github.com/solidsnack/wcwidth/issues/1

module Data.Char.WCWidth
    ( wcwidth
    , wcswidth
    )
where

import Foreign.C

{-| Binding to the native 'wcwidth'.
 -}
wcwidth :: Char -> Int
wcwidth = fromEnum . c_wcwidth . toEnum . fromEnum

-- It'd be nice if this were in the wcwidth package.
wcswidth :: String -> Int
wcswidth = foldr (+) 0 . map wcwidth

foreign import ccall unsafe "wchar.h wcwidth" c_wcwidth :: CWchar -> CInt
