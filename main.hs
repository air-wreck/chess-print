import System.IO

data Color = White | Black deriving (Show, Eq)
-- TODO: consider language selection for piece abbreviations
-- TODO: consider upgrading to Unicode symbols if available
data Type = P | N | B | R | Q | K deriving (Show, Eq)
data Square = Piece Color Type | None deriving (Show, Eq)

print_square :: Square -> String
print_square None = "   "
print_square (Piece White t) = "(" ++ (show t) ++ ")"
print_square (Piece Black t) = "[" ++ (show t) ++ "]"

row_div :: String
row_div = '+' : (concat . (take 8) . repeat $ "---+")

print_row :: [Square] -> String
print_row = foldl (\acc -> (++ "|") . (acc ++) . print_square) "|"

print_board :: [[Square]] -> String
print_board =
  let rd = row_div ++ "\n" in
      foldr (\row acc -> acc ++ ((print_row row) ++ "\n") ++ rd) rd

start_board =
 [[Piece White R, Piece White N, Piece White B, Piece White Q,
   Piece White K, Piece White B, Piece White N, Piece White R],
  (take 8) . repeat $ Piece White P] ++
 ((take 4) . repeat $ (take 8) . repeat $ None) ++
 [(take 8) . repeat $ Piece Black P,
  [Piece Black R, Piece Black N, Piece Black B, Piece Black Q,
   Piece Black K, Piece Black B, Piece Black N, Piece Black R]]

main = do
  putStr "Enter game in algebraic notation => "
  hFlush stdout
  -- TODO: replace this with a stronger input subroutine that
  --       allows multiline input
  --       also consider making the interactive mode a move-by-move loop
  --       and instead offer a separate batch mode?
  --       or it might be more elegant to recognize #. as a move separator and
  --       allow any number of moves to be entered
  game_str <- getLine
  putStrLn $ print_board start_board

