-- SnakeGame.hs

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)

-- Position on the board
data Position = Position Int Int deriving (Eq, Show)

data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Eq, Show)

data Snake = Snake
  { body      :: [Position]
  , direction :: Direction
  } deriving (Show)

newtype Food = Food Position deriving (Show, Eq)

data GameState = GameState
  { snake      :: Snake
  , food       :: Food
  , score      :: Int
  , isGameOver :: Bool
  , rng        :: StdGen
  }

-- Board size
boardWidth, boardHeight, cellSize :: Int
boardWidth = 20
boardHeight = 20
cellSize = 20

-- Initial State
initialState :: StdGen -> GameState
initialState gen = GameState
  { snake = Snake [Position 10 10] RightDir
  , food = Food (Position 5 5)
  , score = 0
  , isGameOver = False
  , rng = gen
  }

-- Move logic
nextPosition :: Position -> Direction -> Position
nextPosition (Position x y) dir = case dir of
  UpDir    -> Position x (y + 1)
  DownDir  -> Position x (y - 1)
  LeftDir  -> Position (x - 1) y
  RightDir -> Position (x + 1) y

moveSnake :: Snake -> Snake
moveSnake (Snake (h:body) dir) = Snake newBody dir
  where newHead = nextPosition h dir
        newBody = newHead : init (h:body)
moveSnake s = s

growSnake :: Snake -> Snake
growSnake (Snake b dir) = Snake (newHead : b) dir
  where newHead = nextPosition (head b) dir

hasSelfCollision :: [Position] -> Bool
hasSelfCollision (h:body) = h `elem` body
hasSelfCollision _ = False

isInsideBounds :: Position -> Bool
isInsideBounds (Position x y) =
  x >= 0 && x < boardWidth && y >= 0 && y < boardHeight

spawnFood :: [Position] -> StdGen -> (Food, StdGen)
spawnFood occupied gen =
  let empty = [Position x y | x <- [0..boardWidth-1], y <- [0..boardHeight-1], Position x y `notElem` occupied]
      (i, gen') = randomR (0, length empty - 1) gen
  in (Food (empty !! i), gen')

updateGame :: Float -> GameState -> GameState
updateGame _ gs@(GameState _ _ _ True _) = gs  -- Game over
updateGame _ gs@(GameState snake@(Snake b dir) (Food f) score _ gen) =
  let nextHead = nextPosition (head b) dir
      ateFood = nextHead == f
      newSnake = if ateFood then growSnake snake else moveSnake snake
      newBody = body newSnake
      (newFood, gen') = if ateFood then spawnFood newBody gen else (Food f, gen)
      collision = not (isInsideBounds nextHead) || hasSelfCollision newBody
      newScore = if ateFood then score + 1 else score
  in GameState newSnake newFood newScore collision gen'

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey key) Down _ _) gs@(GameState s@(Snake b dir) f sc over gen) =
  let newDir = case key of
        KeyUp    -> Just UpDir
        KeyDown  -> Just DownDir
        KeyLeft  -> Just LeftDir
        KeyRight -> Just RightDir
        _        -> Nothing
  in case newDir of
       Just d | not (isOpposite d dir) -> gs { snake = s { direction = d } }
       _ -> gs
handleEvent _ gs = gs

isOpposite :: Direction -> Direction -> Bool
isOpposite UpDir DownDir = True
isOpposite DownDir UpDir = True
isOpposite LeftDir RightDir = True
isOpposite RightDir LeftDir = True
isOpposite _ _ = False

drawGame :: GameState -> Picture
drawGame (GameState (Snake b _) (Food f) _ over _) =
  Pictures (gridLines ++ snakePics ++ [foodPic] ++ gameOverText)
  where
    toScreen (Position x y) = (fromIntegral x * cellSize' - offsetX, fromIntegral y * cellSize' - offsetY)
    cellSize' = fromIntegral cellSize
    -- center of the board is 0, 0
    offsetX = fromIntegral ((boardWidth - 1) * cellSize) / 2
    offsetY = fromIntegral ((boardHeight - 1) * cellSize) / 2 

    drawCell pos col = Translate x y $ Color col $ rectangleSolid cellSize' cellSize'
      where (x, y) = toScreen pos

    snakePics = map (`drawCell` green) b
    foodPic = drawCell f red
    gameOverText = if over
                   then [Translate (-100) 0 $ Scale 0.3 0.3 $ Color white $ Text "Game Over"]
                   else []

    gridLines = [] -- Optional: add grid if needed

main :: IO ()
main = do
  gen <- getStdGen
  play
    (InWindow "Snake" (boardWidth * cellSize, boardHeight * cellSize) (100, 100))
    black          -- background
    10             -- FPS
    (initialState gen)
    drawGame       -- render
    handleEvent    -- input handler
    updateGame     -- update function
