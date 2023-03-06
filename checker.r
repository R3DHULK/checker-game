# Set up the initial board
board <- matrix(0, nrow = 8, ncol = 8)
board[c(1,3,5,7), 2:8:2] <- 1
board[c(2,4,6,8), 1:8:2] <- 1

# Define a function to print the board
print_board <- function(board) {
  for (i in 8:1) {
    row <- paste0(i, " ")
    for (j in 1:8) {
      if (board[i, j] == 1) {
        row <- paste0(row, "O ")
      } else if (board[i, j] == 2) {
        row <- paste0(row, "X ")
      } else {
        row <- paste0(row, ". ")
      }
    }
    cat(row, "\n")
  }
  cat("  a b c d e f g h\n")
}

# Define a function to move a piece
move_piece <- function(board, from, to) {
  # Convert the input strings to row and column indices
  from_row <- 9 - as.integer(strsplit(from, "")[[1]][2])
  from_col <- match(strsplit(from, "")[[1]][1], letters)
  to_row <- 9 - as.integer(strsplit(to, "")[[1]][2])
  to_col <- match(strsplit(to, "")[[1]][1], letters)
  
  # Check if the move is valid
  if (board[from_row, from_col] != 1) {
    cat("Invalid move: there is no piece at", from, "\n")
    return(board)
  } else if (to_row > from_row) {
    cat("Invalid move: you can only move pieces diagonally upwards\n")
    return(board)
  } else if (to_row == from_row | abs(to_col - from_col) != 1) {
    cat("Invalid move: you can only move one square diagonally\n")
    return(board)
  } else if (board[to_row, to_col] != 0) {
    cat("Invalid move: that square is already occupied\n")
    return(board)
  }
  
  # Move the piece
  board[from_row, from_col] <- 0
  board[to_row, to_col] <- 1
  
  # Check if the piece should be crowned
  if (to_row == 8) {
    board[to_row, to_col] <- 2
    cat("Crowned a piece!\n")
  }
  
  return(board)
}

# Play the game
print_board(board)
while (TRUE) {
  from <- readline(prompt = "Enter the square of the piece to move: ")
  to <- readline(prompt = "Enter the square to move to: ")
  board <- move_piece(board, from, to)
  print_board(board)
}
