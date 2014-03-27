{-# LANGUAGE CPP #-}

import Control.Monad (liftM)
import Data.Function (on)
import Data.List (intersperse, sortBy, elemIndex)
import Data.Maybe (fromJust, listToMaybe)
import System.Environment (getArgs)


main :: IO ()
main = do
    a <- liftM (map read) getArgs
    putStrLn . show . winsnap (snapConfigs !! (a!!0))
        $ Rect (a!!1) (a!!2) (a!!3) (a!!4)


data Rect a = Rect
    { rectX :: a
    , rectY :: a
    , rectW :: a
    , rectH :: a
    } deriving (Eq)


type Window = Rect Int
type Monitor = Rect Int
type SnapConfig = Rect Float


instance (Show a) => Show (Rect a) where
    show (Rect x y w h) = concat . intersperse " " . map show $ [x,y,w,h]


panelSize :: Int
panelSize = 24


monitors :: [Monitor]
monitors = map parseMonitor [
#include "monitors.txt"
  ]
  where
    parseMonitor (w,h,x,y) = Rect x y w (h - panelSize)


snapConfigs :: [[SnapConfig]]
snapConfigs =
    -- Top Left
    [ [ Rect 0.0 0.0 0.3333 0.5
      , Rect 0.0 0.0 0.5    0.5
      , Rect 0.0 0.0 0.6666 0.5 ]
    -- Top Center
    , [ Rect 0.0    0.0 1.0    0.5
      , Rect 0.1667 0.0 0.6666 0.5
      , Rect 0.3333 0.0 0.3333 0.5 ]
    -- Top Right
    , [ Rect 0.6666 0.0 0.3333 0.5
      , Rect 0.5    0.0 0.5    0.5
      , Rect 0.3333 0.0 0.6666 0.5 ]
    -- Center Left
    , [ Rect 0.0 0.0 0.3333 1.0
      , Rect 0.0 0.0 0.5    1.0
      , Rect 0.0 0.0 0.6666 1.0 ]
    -- Center Center
    , [ Rect 0.0    0.0 1.0    1.0
      , Rect 0.1667 0.0 0.6666 1.0
      , Rect 0.3333 0.0 0.3333 1.0 ]
    -- Center Right
    , [ Rect 0.6666 0.0 0.3333 1.0
      , Rect 0.5    0.0 0.5    1.0
      , Rect 0.3333 0.0 0.6666 1.0 ]
    -- Bottom Left
    , [ Rect 0.0 0.5 0.3333 0.5
      , Rect 0.0 0.5 0.5    0.5
      , Rect 0.0 0.5 0.6666 0.5 ]
    -- Bottom Center
    , [ Rect 0.0    0.5 1.0    0.5
      , Rect 0.1667 0.5 0.6666 0.5
      , Rect 0.3333 0.5 0.3333 0.5 ]
    -- Bottom Right
    , [ Rect 0.6666 0.5 0.3333 0.5
      , Rect 0.5    0.5 0.5    0.5
      , Rect 0.3333 0.5 0.6666 0.5 ] ]



winsnap :: [SnapConfig] -> Window -> Window
winsnap snaps win = applySnap mon newSnap win
  where
    mon = fromJust $ whichMonitor win monitors
    close = closestSnap mon snaps win
    index = fromJust $ elemIndex close snaps
    nextIndex = if index == length snaps - 1 then 0 else index + 1
    newSnap = snaps !! nextIndex


whichMonitor :: Window -> [Monitor] -> Maybe Monitor
whichMonitor win mons =
    listToMaybe $ filter onMon mons
  where
    rectIntCenter (Rect x y w h) = (x + w `quot` 2 , y + h `quot` 2)
    (wx, wy) = rectIntCenter win
    onMon (Rect mx my mw mh) = wx >= mx && wx < mx + mw
                            && wy >= my && wy < my + mh


closestSnap :: Monitor -> [SnapConfig] -> Window -> SnapConfig
closestSnap mon snaps win =
    fst . head . sortBy (compare `on` snd) . map zipDistance $ snaps
  where
    zipDistance snap = (snap, (abs (x - rectX win) + abs (y - rectY win)
                             + abs (w - rectW win) + abs (h - rectH win)))
      where (Rect x y w h) = applySnap mon snap win


applySnap :: Monitor -> SnapConfig -> Window -> Window
applySnap (Rect mx my mw mh) (Rect sx sy sw sh) (Rect wx wy ww wh) =
    Rect x y w h
  where
    x = mx + round ((fromIntegral mw) * sx)
    y = my + round ((fromIntegral mh) * sy)
    w = round ((fromIntegral mw) * sw)
    h = round ((fromIntegral mh) * sh)


