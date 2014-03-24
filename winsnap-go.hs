
import Control.Monad (liftM)
import Data.Function (on)
import Data.List (intersperse, sortBy, elemIndex)
import Data.Maybe (fromJust, listToMaybe)
import System.Environment (getArgs)


main :: IO ()
main = do
    a <- liftM (map read) getArgs
    putStrLn . show . winsnap snapsTopLeft $ Rect (a!!0) (a!!1) (a!!2) (a!!3)


data Rect = Rect
    { rectX :: Int
    , rectY :: Int
    , rectW :: Int
    , rectH :: Int
    } deriving (Eq)

type Window = Rect
type Monitor = Rect

instance Show Rect where
    show (Rect x y w h) = concat . intersperse " " . map show $ [x,y,w,h]

data SnapConfig = SnapConfig
    { snapX :: Float
    , snapY :: Float
    , snapW :: Float
    , snapH :: Float
    } deriving (Eq)


monitors :: [Monitor]
monitors =
    [ Rect    0  360 1920 1080
    , Rect 1920    0 2560 1440
    , Rect 4480    0 2560 1440 ]

snapsTopLeft :: [SnapConfig]
snapsTopLeft =
    [ SnapConfig 0.0 0.0 0.3333 0.5
    , SnapConfig 0.0 0.0 0.5    0.5
    , SnapConfig 0.0 0.0 0.6666 0.5 ]


rectCenter :: Rect -> (Int, Int)
rectCenter (Rect x y w h) = (x + w `quot` 2 , y + h `quot` 2)


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
    (wx, wy) = rectCenter win
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
applySnap (Rect mx my mw mh) (SnapConfig sx sy sw sh) (Rect wx wy ww wh) =
    Rect x y w h
  where
    x = mx + round ((fromIntegral (wx - mx)) * sx)
    y = my + round ((fromIntegral (wy - my)) * sy)
    w = round ((fromIntegral mw) * sw)
    h = round ((fromIntegral mh) * sh)



