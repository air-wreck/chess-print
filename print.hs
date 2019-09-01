import System.IO
import Data.List.Split (splitOn)

data Color  = White | Black deriving Eq
data Type   = Pawn | Knight | Bishop | Rook | Queen | King deriving Eq
data Square = Piece Color Type | None deriving Eq

type Board  = [[Square]]
type State  = (Board, PFlags, KFlags)

{--- BOARD DISPLAY ---}

instance Show Square where
  show  None                = "   "
  show (Piece White Pawn)   = " ♙ "
  show (Piece White Knight) = " ♘ "
  show (Piece White Bishop) = " ♗ "
  show (Piece White Rook)   = " ♖ "
  show (Piece White Queen)  = " ♕ "
  show (Piece White King)   = " ♔ "
  show (Piece Black Pawn)   = " ♟ "
  show (Piece Black Knight) = " ♞ "
  show (Piece Black Bishop) = " ♝ "
  show (Piece Black Rook)   = " ♜ "
  show (Piece Black Queen)  = " ♛ "
  show (Piece Black King)   = " ♚ "

template = unlines [
  "┌───┬───┬───┬───┬───┬───┬───┬───┐",
  "│ * │ * │ * │ * │ * │ * │ * │ * │",
  "├───┼───┼───┼───┼───┼───┼───┼───┤",
  "│ * │ * │ * │ * │ * │ * │ * │ * │",
  "├───┼───┼───┼───┼───┼───┼───┼───┤",
  "│ * │ * │ * │ * │ * │ * │ * │ * │",
  "├───┼───┼───┼───┼───┼───┼───┼───┤",
  "│ * │ * │ * │ * │ * │ * │ * │ * │",
  "├───┼───┼───┼───┼───┼───┼───┼───┤",
  "│ * │ * │ * │ * │ * │ * │ * │ * │",
  "├───┼───┼───┼───┼───┼───┼───┼───┤",
  "│ * │ * │ * │ * │ * │ * │ * │ * │",
  "├───┼───┼───┼───┼───┼───┼───┼───┤",
  "│ * │ * │ * │ * │ * │ * │ * │ * │",
  "├───┼───┼───┼───┼───┼───┼───┼───┤",
  "│ * │ * │ * │ * │ * │ * │ * │ * │",
  "└───┴───┴───┴───┴───┴───┴───┴───┘"]

{- if your environment does not support Unicode, or if the Unicode characters
   are difficult to see on your display, you may prefer the following

instance Show Square where
  show  None                = "   "
  show (Piece White Pawn)   = "(P)"
  show (Piece White Knight) = "(N)"
  show (Piece White Bishop) = "(B)"
  show (Piece White Rook)   = "(R)"
  show (Piece White Queen)  = "(Q)"
  show (Piece White King)   = "(K)"
  show (Piece Black Pawn)   = "[P]"
  show (Piece Black Knight) = "[N]"
  show (Piece Black Bishop) = "[B]"
  show (Piece Black Rook)   = "[R]"
  show (Piece Black Queen)  = "[Q]"
  show (Piece Black King)   = "[K]"

template = unlines [
  "+---+---+---+---+---+---+---+---+",
  "| * | * | * | * | * | * | * | * |",
  "+---+---+---+---+---+---+---+---+",
  "| * | * | * | * | * | * | * | * |",
  "+---+---+---+---+---+---+---+---+",
  "| * | * | * | * | * | * | * | * |",
  "+---+---+---+---+---+---+---+---+",
  "| * | * | * | * | * | * | * | * |",
  "+---+---+---+---+---+---+---+---+",
  "| * | * | * | * | * | * | * | * |",
  "+---+---+---+---+---+---+---+---+",
  "| * | * | * | * | * | * | * | * |",
  "+---+---+---+---+---+---+---+---+",
  "| * | * | * | * | * | * | * | * |",
  "+---+---+---+---+---+---+---+---+",
  "| * | * | * | * | * | * | * | * |",
  "+---+---+---+---+---+---+---+---+"]
-}

-- for reference, this is the standard starting configuration of the board
start_board :: Board
start_board =
  [[Piece White Rook, Piece White Knight, Piece White Bishop, Piece White Queen,
    Piece White King, Piece White Bishop, Piece White Knight, Piece White Rook],
   (take 8) . repeat $ Piece White Pawn] ++
  ((take 4) . repeat $ (take 8) . repeat $ None) ++
  [(take 8) . repeat $ Piece Black Pawn,
   [Piece Black Rook, Piece Black Knight, Piece Black Bishop, Piece Black Queen,
    Piece Black King, Piece Black Bishop, Piece Black Knight, Piece Black Rook]]
start_kflags :: KFlags
start_kflags = ((False, False, False), (False, False, False))
start_pflags :: PFlags
start_pflags = (take 8 $ repeat False, take 8 $ repeat False)
start :: State
start = (start_board, start_pflags, start_kflags)

-- provide a string representation of a State
-- replace every '*' in the template with a square from the board
toStr :: State -> String
toStr (board, _, _) = (++) (head lines)
  $ foldl (\acc (x,y) -> acc ++ x ++ y) [] $ zip board' (tail lines)
  where flatten = foldl (++) []
        board'  = map show $ flatten $ reverse board
        lines   = splitOn " * " template

{--- MOVE HANDLING ---}

piece :: String -> Square
piece "WP" = Piece White Pawn
piece "WN" = Piece White Knight
piece "WB" = Piece White Bishop
piece "WR" = Piece White Rook
piece "WQ" = Piece White Queen
piece "WK" = Piece White King
piece "BP" = Piece Black Pawn
piece "BN" = Piece Black Knight
piece "BB" = Piece Black Bishop
piece "BR" = Piece Black Rook
piece "BQ" = Piece Black Queen
piece "BK" = Piece Black King
piece _ = None

-- convert "a1" -> (0,0)
rank :: String -> Int
rank coords = read (tail coords) - 1
file :: String -> Int
file coords = snd . head . filter ((==(head coords)).fst) $ zip "abcdefgh" [0..]

-- convert (0,0) -> "a1"
a1 :: (Int, Int) -> String
a1 (rank, file) = ("abcdefgh" !! file) : (show . (+1)) rank

-- get the current contents of a board coordinate like "a1"
get_sq :: Board -> String -> Square
get_sq board coords =
  board !! (rank coords) !! (file coords)

-- set a board coordinate like "a1" to the given piece
-- e.g. set_sq (Piece White Pawn) "e4" start_board
set_sq :: Square -> String -> Board -> Board
set_sq square coords board =
  foldr (\(rank', row) acc ->
    (if rank' /= rank
     then (map snd row)
     else foldr (\(file', sq) row' ->
       (if file' /= file
        then sq
        else square)
       : row') [] row) : acc) [] board'
  where
    -- annotate board with coordinate information
    board' = zip "12345678" $ map (zip "abcdefgh") board
    rank = last coords
    file = head coords

-- process a move of the form: (move "e2" "e4" start)
move :: String -> String -> State -> State
move from to (board, pf, kf) =
  (board', pf', kf')
  where
    coords = (rank from, file from)
    -- update the board by moving the piece to the next square
    board' = set_sq (get_sq board from) to $ (set_sq None from) board
    -- mark if any pawn made a double move this turn
    let piece_to_move = get_sq board from
    in  pf' = if piece_to_move == Piece White Pawn
              then setPF pf "a" (+2)
              else
    -- mark if any kings or rooks have moved so far
    combine = ((map and) .) . zip
    let piece_to_move = get_sq board from
    in  kf' = combine kf $
                case piece_to_move of Piece White King -> ((True

-- return a list of the coords to which the piece on the given coords can move
-- due to en passant captures, 'pawn previously doubled' flags are passed
--   the format is ([Bool], [Bool]) for (white, black) along the files [a..h]
-- due to castles, 'king/rook moved' flags must be passed
--   the format is ((Bool, Bool, Bool), (Bool, Bool, Bool)) for (white, black)
--   for (king, king's rook, queen's rook)
-- e.g. legal_moves "e2" start_board dummypf dummykf start_board => ["e3", "e4"]
type PFlags = ([Bool], [Bool])
dummypf = (take 8 $ repeat False, take 8 $ repeat False)
type KFlags = ((Bool, Bool, Bool), (Bool, Bool, Bool))
dummykf = ((False, False, False), (False, False, False))
legal_moves :: String -> PFlags -> KFlags -> Board -> [String]
legal_moves coords pflags kflags board =
  map a1 $ filter no_color $ filter in_board $
    legal (get_sq board coords) coords pflags kflags board
  where
    rank' = rank coords
    file' = file coords
    legal None _ _ _ _ = []
    legal (Piece White Pawn) coords pflags kflags board =
      -- pawns can move one space forward if empty
      -- pawns can move two spaces forward if both empty and on second rank
      (let fwd1 = (rank' + 1, file')
           fwd2 = (rank' + 2, file')
       in  (if empty fwd1 then [fwd1] else []) ++
           (if empty fwd1 && empty fwd2 && rank' == 1 then [fwd2] else [])) ++
      -- pawns can capture on the diagonal
      (let diag_left  = (rank' + 1, file' - 1)
           diag_right = (rank' + 1, file' + 1)
       in  (if (not.empty) diag_left then [diag_left] else []) ++
           (if (not.empty) diag_right then [diag_right] else []))
      -- pawns can capture en passant if on the second rank
    legal (Piece Black Pawn) coords pflags _ board =
      []
    legal (Piece _ Knight) coords _ _ board =
      -- knights can jump anywhere without restriction
      [(rank'+2, file'+1), (rank'+2, file'-1), (rank'-2, file'+1),
       (rank'-2, file'-1), (rank'+1, file'+2), (rank'+1, file'-2),
       (rank'-1, file'+2), (rank'-1, file'-2)]
    legal (Piece _ Bishop) coords _ _ board =
      -- bishops can move diagonally until they hit another piece
      flatten $ map ((takeUntil (not.empty)) . apply)
       [\(r,f) -> (r+1,f+1),
        \(r,f) -> (r+1,f-1),
        \(r,f) -> (r-1,f+1),
        \(r,f) -> (r-1,f-1)]
    legal (Piece _ Rook) coords _ _ board =
      -- rooks can move in straight lines until they hit another piece
      flatten $ map ((takeUntil (not.empty)) . apply)
       [\(r,f) -> (r+1,f),
        \(r,f) -> (r,f+1),
        \(r,f) -> (r-1,f),
        \(r,f) -> (r,f-1)]
    legal (Piece _ Queen) coords _ _ board =
      -- queens can move like a bishop or a rook
      (legal (Piece White Bishop) coords dummypf dummykf board) ++
      (legal (Piece White Rook  ) coords dummypf dummykf board)
    legal (Piece White King) coords _ ((king, krook, qrook), _) board =
      -- the king can move one square in any direction
      [(r,f) | r <- [rank' - 1 .. rank' + 1], f <- [file' - 1 .. file' + 1]]
      -- the king can castle on first move with an original rook
      -- we must check all intervening spaces for pieces and checks
      --
      -- the king cannot move into check
    legal (Piece Black King) coords _ (_, (king, krook, qrook)) board =
      []

    -- determine if a coordinate is in the board
    in_board (r,f) =
      (elem r [0..7]) && (elem f [0..7])
    -- determine if a coordinate is empty
    empty coords =
      if in_board coords
      then (== None) $ get_sq board (a1 coords)
      else True
    -- determine if there is no color conflict (piece of the same color)
    no_color (r,f) =
      if empty (rank', file') || empty (r, f)
      then True
      else let color (Piece col _) = col
           in  (color $ get_sq board coords) /= (color $ get_sq board $ a1 (r,f))
    -- flatten a list
    flatten = foldl (++) []
    -- obviously, don't do this on an infinite list
    takeUntil _ [] = []
    takeUntil p (x:xs) = if p x then [x] else x : (takeUntil p xs)
    -- apply a move (r,f) -> (r',f')
    apply f = take 8 $ tail $ iterate f (rank', file')

{--- INPUT LOOP ---}

main = do
  putStr "Enter move(s) in algebraic notation => "
  hFlush stdout
  game_str <- getLine
  -- some processing here
  putStrLn $ toStr start
