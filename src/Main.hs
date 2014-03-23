{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import FreeGame
-- import Linear

{-
Window size: 480 x 360
Game size: 50 x 50
Cell size: 6 x 6
-}

type Coord = V2 Int

data StaticEntity =
  Wall

data Position = Transit {_start :: Coord, _dest :: Coord}
              deriving (Eq, Ord)
makeLenses ''Position

data Zombie = Zombie {
  _position :: Position
  } deriving (Eq, Ord)

type GameMap = Map Coord (Either StaticEntity Zombie)
-- invariant that the coord of a zombie in the gamemap is equal to the
-- dest of the zombie's position

data GameState = GameState {
  _gameMap :: GameMap,
  _target :: Position,
  _ticker :: Int -- ticker `elem` [0,16)
  }
makeLenses ''GameState

drawStaticEntity :: Picture2D p => Coord -> StaticEntity -> p ()
drawStaticEntity coord Wall =
  color green $ polygon $ map ((*) 12 . fmap fromIntegral)
  [coord, coord & _x +~ 1, coord & _y +~ 1, coord & traverse +~ 1]

drawZombie :: Picture2D p => Bitmap -> Int -> Zombie -> p ()
drawZombie zombieSprite t Zombie {_position = p} =
  let dvec = fmap fromIntegral $ p ^. dest - p ^. start
      angle = view _y dvec `atan2` view _x dvec
      pos = p ^.(start.to (fmap fromIntegral)) + (fromIntegral t / 16) *^ dvec
  in rotateR angle $ translate (12*pos) $ bitmap zombieSprite

drawGame :: (Monad p, Picture2D p) => Bitmap -> GameState -> p ()
drawGame zombieSprite gameState = do
  imapMOf_ (gameMap . ifolded <. _Left) drawStaticEntity gameState
  let drawZombie1 = drawZombie zombieSprite $ gameState ^. ticker
  mapMOf_ (gameMap . folded . _Right) drawZombie1 gameState

loadMap :: FilePath -> IO GameMap
loadMap path = do
  things <- path ^@!! act readFile . to lines . ifolded <.> ifolded
  return $ M.fromList $ do
    (c,t) <- things
    let v = uncurry (flip V2) c
    Just e <- [convert v t]
    return (v, e)
  where
    convert _ '#' = Just $ Left Wall
    convert c 'Z' =
      Just $ Right Zombie {_position = Transit {_start = c, _dest = c}}
    convert _ _ = Nothing

main :: IO ()
main = do
  m <- loadMap "data/map1.map"
  z <- readBitmap "data/zombie.png"
  _ <- runGame Windowed (BoundingBox 0 0 600 600) $ foreverFrame $ drawGame z $
    GameState {_gameMap = m, _target = undefined, _ticker = 0}
  return ()