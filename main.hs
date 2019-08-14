import System.IO
import Data.Char (isSpace)
import qualified Data.Text as Txt

data Color = White | Black deriving (Show, Eq)
-- TODO: consider language selection for piece abbreviations
-- TODO: consider upgrading to Unicode symbols if available
data Type = P | N | B | R | Q | K deriving (Show, Eq)
data Square = Piece Color Type | None deriving (Show, Eq)

print_square :: Square -> String
print_square None = "   "
print_square (Piece White t) = "(" ++ (show t) ++ ")"
print_square (Piece Black t) = "[" ++ (show t) ++ "]"

-- TODO: consider using Unicode pipes if available
row_div :: String
row_div = '+' : (concat . (take 8) . repeat $ "---+")

print_row :: [Square] -> String
print_row = foldl (\acc -> (++ "|") . (acc ++) . print_square) "|"

print_board :: [[Square]] -> String
print_board =
  let rd = row_div ++ "\n" in
      foldr (\row acc -> acc ++ ((print_row row) ++ "\n") ++ rd) rd

-- strip notational comments (!, ?, TN) from a move
strip_A1_move :: String -> String
strip_A1_move move_raw =
  let comments = map Txt.pack ["!", "?", "TN"]
      move = Txt.pack move_raw
  in Txt.unpack $ foldr (flip Txt.replace Txt.empty) move comments

-- accept the current board, the player to move, and a single move
-- in algebraic notation and return an explicit "move description"
--
-- the description consists of a sequence of space-delimited changes
-- to be made to the board; for example:
--   "e4" -> "C[b4] wP[e4]" (clear b4, place a white pawn on e4)
parse_A1_move :: [[Square]] -> Color -> String -> String
parse_A1_move board to_move move_raw = 
  let move = (Txt.pack . strip_A1_move) move_raw
      player = if to_move == White
                 then Txt.pack "w"
               else
                 Txt.pack "b"
  in Txt.unpack move

-- accept a board, a move string describing the changes, and return new board
-- do_move :: [[Square]] -> String -> [String]
-- TEMPORARY implementation with just an explicit "move description"
-- do_move board move =
--  let mv = (Txt.splitBy isSpace . Txt.pack) move
--  in map Txt.unpack mv

start_board =
 [[Piece White R, Piece White N, Piece White B, Piece White Q,
   Piece White K, Piece White B, Piece White N, Piece White R],
  (take 8) . repeat $ Piece White P] ++
 ((take 4) . repeat $ (take 8) . repeat $ None) ++
 [(take 8) . repeat $ Piece Black P,
  [Piece Black R, Piece Black N, Piece Black B, Piece Black Q,
   Piece Black K, Piece Black B, Piece Black N, Piece Black R]]

main = do
  putStr "Enter move(s) in algebraic notation => "
  hFlush stdout
  -- TODO: replace this with a stronger input subroutine that
  --       allows multiline input
  --       also consider making the interactive mode a move-by-move loop
  --       and instead offer a separate batch mode?
  --       or it might be more elegant to recognize #. as a move separator and
  --       allow any number of moves to be entered
  --
  --       so, this might work by reading input line-by-line, where each
  --       line can contain an unlimited number of (#.) delimited moves
  --       the program stops taking input when EOF is reached, allowing
  --       a file to be piped in over stdin or manual input to terminate with
  --       a Ctrl+D
  game_str <- getLine
  putStrLn $ print_board start_board

