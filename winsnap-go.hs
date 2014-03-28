{-# LANGUAGE CPP #-}

import Control.Monad (liftM)
import Data.Function (on)
import Data.List (intersperse, sortBy, elemIndex)
import Data.Maybe (fromJust, listToMaybe, fromMaybe)
import System.Environment (getArgs)

--------------------------------------------------------------------------------

-- Basic rectangle data type
data Rect a = Rect
    { rectX :: a
    , rectY :: a
    , rectW :: a
    , rectH :: a
    } deriving (Eq)

instance (Show a) => Show (Rect a) where
    show (Rect x y w h) = concat . intersperse " " . map show $ [x,y,w,h]


-- Represents a monitor's position and size in the full desktop area.
type Monitor = Rect Int

-- Represents pixel-based window geometry in the full desktop area.
type FixedWindow = Rect Int

-- Represents a monitor-independent window geometry. Position and
-- size are given as fractions of a monitor rather than pixels.
type GeneralWindow = Rect Float

--------------------------------------------------------------------------------

-- Finds an item in a list, and cycles to the next item.
getNext :: Eq a => [a] -> a -> a
getNext []  _ = error "Cannot get next item in empty list"
getNext [x] _ = x
getNext list item = list !! nextIndex
  where
    index = fromMaybe 0 $ elemIndex item list
    nextIndex = if index == length list - 1 then 0 else index + 1

--------------------------------------------------------------------------------

panelSize :: Int
panelSize = 24

monitors :: [Monitor]
monitors = map parseMonitor [
#include "monitors.txt"
  ]
  where
    parseMonitor (w,h,x,y) = Rect x y w (h - panelSize)

snapConfigs :: [[GeneralWindow]]
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

--------------------------------------------------------------------------------

main :: IO ()
main = dispatch `liftM` getArgs >>= putStrLn


dispatch :: [String] -> String
dispatch args = case head args of

    "snap" -> show $ snapNext (snapConfigs!!(ints!!0))
                   $ Rect (ints!!1) (ints!!2) (ints!!3) (ints!!4)

    "next" -> show $ nextMonitor monitors
                   $ Rect (ints!!0) (ints!!1) (ints!!2) (ints!!3)

    x -> error $ "Didn't understand command '" ++ x ++ "'"

  where
    ints = map read $ tail args

--------------------------------------------------------------------------------

-- When applied to a pixel-based window geometry, this function returns a
-- resolution-independent window geometry relative to the given monitor.
getGeneral :: Monitor -> FixedWindow -> GeneralWindow
getGeneral (Rect mx my mw mh) (Rect fx fy fw fh) =
    Rect gx gy gw gh
  where
    gx = fromIntegral (fx - mx) / fromIntegral mw
    gy = fromIntegral (fy - my) / fromIntegral mh
    gw = fromIntegral fw / fromIntegral mw
    gh = fromIntegral fh / fromIntegral mh


-- When applied to a specific monitor and a resolution-independent window
-- geometry, this returns a conrete window geometry.
getFixed :: Monitor -> GeneralWindow -> FixedWindow
getFixed (Rect mx my mw mh) (Rect gx gy gw gh) =
    Rect fx fy fw fh
  where
    fx = (mx +) $ round $ fromIntegral mw * gx
    fy = (my +) $ round $ fromIntegral mh * gy
    fw = round $ fromIntegral mw * gw
    fh = round $ fromIntegral mh * gh


-- Given a list of monitors and a fixed window position, this function returns
-- a window geometry describing the window after being moved to the next monitor
-- in the list.
nextMonitor :: [Monitor] -> FixedWindow -> FixedWindow
nextMonitor mons win =
    getFixed nextMon $ getGeneral thisMon win
  where
    thisMon = fromJust $ whichMonitor mons win
    nextMon = getNext mons thisMon


-- Determines which monitor a given window resides on.
whichMonitor :: [Monitor] -> FixedWindow -> Maybe Monitor
whichMonitor mons win =
    listToMaybe $ filter onMon mons
  where
    rectIntCenter (Rect x y w h) = (x + w `quot` 2 , y + h `quot` 2)
    (wx, wy) = rectIntCenter win
    onMon (Rect mx my mw mh) = wx >= mx && wx < mx + mw
                            && wy >= my && wy < my + mh



snapNext :: [GeneralWindow] -> FixedWindow -> FixedWindow
snapNext snaps win = getFixed mon newSnap
  where
    mon = fromJust $ whichMonitor monitors win
    newSnap = getNext snaps (closestSnap mon snaps win)


closestSnap :: Monitor -> [GeneralWindow] -> FixedWindow -> GeneralWindow
closestSnap mon snaps win =
    fst . head . sortBy (compare `on` snd) . map zipDistance $ snaps
  where
    zipDistance snap = (snap, (abs (x - rectX win) + abs (y - rectY win)
                             + abs (w - rectW win) + abs (h - rectH win)))
      where (Rect x y w h) = getFixed mon snap

