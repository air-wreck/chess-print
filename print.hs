import Data.List.Split (splitOn)

data Color  = White | Black
data Type   = Pawn | Knight | Bishop | Rook | Queen | King
data Square = Piece Color Type | None
type Board  = [[Square]]

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

-- replace every '*' in the template with a square from the board
print_board :: Board -> String
print_board board = (++) (head lines)
  $ foldl (\acc (x,y) -> acc ++ x ++ y) [] $ zip board' (tail lines)
  where flatten = foldl (++) []
        board'  = map show $ flatten $ reverse board
        lines   = splitOn " * " template

