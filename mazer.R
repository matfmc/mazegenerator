library(imager)

# Functions to pre-run ---------------------------------------------------------------------------------

neighbors <- function(x, board_size = board) {
  x <- matrix(c(x[1] + 1, x[2], x[1] - 1, x[2], x[1], x[2] - 1, x[1], x[2] + 1), ncol = 2, byrow = T)
  x <- x[(x[, 1] != 0 & x[, 1] <= nrow(board_size)) & (x[, 2] != 0 & x[, 2] <= ncol(board_size)), ]
  x <- split(x, seq(nrow(x)))
  return(x)
}

walk <- function(posichon, stack, visited) {
  possible_neighbors <- neighbors(posichon)[(neighbors(posichon) %in% visited) == FALSE]
  if (length(possible_neighbors) != 0) {
    next_step <- c(unlist(sample(possible_neighbors, 1), use.names = F))
    stack <- append(list(next_step), stack)
    visited <- append(list(next_step), visited)
  } else {
    next_step <- stack[-1][[1]]
    stack <- stack[-1]
  }

  return(list(next_step = next_step, stack = stack, visited = visited))
}

mazefy <- function(start_coord_x, start_coord_y, board = board) {
  rat <- c(start_coord_x, start_coord_y)
  rat_trail <- list()
  stack <- list()
  visited <- list()
  rat_trail[[1]] <- rat
  stack[[1]] <- rat
  visited[[1]] <- rat

  while (length(visited) < nrow(board) * ncol(board)) {
    next_step <- walk(rat, stack, visited)
    stack <- next_step$stack
    visited <- next_step$visited
    rat <- next_step$next_step
    rat_trail <- append(list(rat), rat_trail)
    print(rat)
    print(length(visited))
  }

  return(rat_trail = rat_trail)
}


conectores <- function(start_coord_x = start_coord_x, start_coord_y = start_coord_y, board = board) {
  walked <- mazefy(start_coord_x , start_coord_y , board)
  walked <- walked[length(walked):1]
  walked <- data.frame(t(do.call(data.frame, walked)), row.names = NULL)

  for (i in 1:nrow(walked)) {
    if (i == 1) {
      walked[i, 3:4] <- (walked[i, 1:2] - walked[i + 1, 1:2])
    } else if (i == nrow(walked)) {
      walked[i, 3:4] <- (walked[i, 1:2] - walked[i - 1, 1:2])
    } else {
      walked[i, 3:4] <- (walked[i, 1:2] - walked[i + 1, 1:2])
      walked[i, 5:6] <- (walked[i, 1:2] - walked[i - 1, 1:2])
    }
  }

  walked[walked[, 3] == -1 & walked[, 4] == 0, "conections"] <- "A"
  walked[walked[, 3] == 0 & walked[, 4] == -1, "conections"] <- "B"
  walked[walked[, 3] == 1 & walked[, 4] == 0, "conections"] <- "C"
  walked[walked[, 3] == 0 & walked[, 4] == 1, "conections"] <- "D"

  walked[(walked[, 5] == -1 & walked[, 6] == 0) & !is.na(walked[, 5]), "conections_2"] <- "A"
  walked[(walked[, 5] == 0 & walked[, 6] == -1) & !is.na(walked[, 5]), "conections_2"] <- "B"
  walked[(walked[, 5] == 1 & walked[, 6] == 0) & !is.na(walked[, 5]), "conections_2"] <- "C"
  walked[(walked[, 5] == 0 & walked[, 6] == 1) & !is.na(walked[, 5]), "conections_2"] <- "D"

  walked$conec <- paste0(walked$conections, walked$conections_2)
  walked <- walked[, c(1, 2, 9)]

  walked[walked$conec == "CD" | walked$conec == "DC" | walked$conec == "DNA" | walked$conec == "CNA" | walked$conec == "CC" | walked$conec == "DD", "cell"] <- 1
  walked[walked$conec == "AC" | walked$conec == "CA" | walked$conec == "AD" | walked$conec == "DA" | walked$conec == "ANA" | walked$conec == "AA", "cell"] <- 2
  walked[walked$conec == "BC" | walked$conec == "CB" | walked$conec == "BD" | walked$conec == "DB" | walked$conec == "BNA" | walked$conec == "BB", "cell"] <- 3
  walked[walked$conec == "AB" | walked$conec == "BA", "cell"] <- 4

  return(walked)
}


labyrinth <- function(start_coord_x = start_coord_x, start_coord_y = start_coord_y, board = board) {
  nu_row <- nrow(board)
  nu_cow <- ncol(board)
  walked <- conectores(start_coord_x , start_coord_y , board)

  for (i in 1:nrow(walked)) {
    if (board[walked$X1[i], walked$X2[i]] == 0) {
      board[walked$X1[i], walked$X2[i]] <- walked$cell[i]
    } else if (board[walked$X1[i], walked$X2[i]] == 4) {
      next
    } else if (board[walked$X1[i], walked$X2[i]] == 2 & walked$cell[i] == 3) {
      board[walked$X1[i], walked$X2[i]] <- 4
    } else if (board[walked$X1[i], walked$X2[i]] == 3 & walked$cell[i] == 2) {
      board[walked$X1[i], walked$X2[i]] <- 4
    } else if (board[walked$X1[i], walked$X2[i]] == 1) {
      board[walked$X1[i], walked$X2[i]] <- walked$cell[i]
    }
  }

  cell_type_0 <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0), ncol = 3, byrow = T)
  cell_type_1 <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 1), ncol = 3, byrow = T)
  cell_type_2 <- matrix(c(0, 0, 1, 0, 0, 1, 0, 0, 1), ncol = 3, byrow = T)
  cell_type_3 <- matrix(c(0, 0, 0, 0, 0, 0, 1, 1, 1), ncol = 3, byrow = T)
  cell_type_4 <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 1), ncol = 3, byrow = T)

  board_2 <- as.list(board)
  dim(board_2) <- c(nrow(board), ncol(board))

  board_2[, ][board_2[, ] %in% 0] <- list(cell_type_0)
  board_2[, ][board_2[, ] %in% 1] <- list(cell_type_1)
  board_2[, ][board_2[, ] %in% 2] <- list(cell_type_2)
  board_2[, ][board_2[, ] %in% 3] <- list(cell_type_3)
  board_2[, ][board_2[, ] %in% 4] <- list(cell_type_4)

  b <- do.call(cbind, board_2[1, 1:nu_cow])
  for (i in 2:nu_row) {
    a <- do.call(cbind, board_2[i, 1:nu_cow])
    b <- rbind(b, a)
  }

  b <- cbind(1, b)
  b <- rbind(1, b)
  b[b == 0] <- 2
  b <- t(b)
  return(b)
}

esticar <- function(matrix, multiplicador) {
  return(matrix[rep(1:nrow(matrix), each = multiplicador), rep(1:ncol(matrix), each = multiplicador)])
}


labyrinth_2 <- function(start_coord_x = start_coord_x, start_coord_y = start_coord_y, board = board) {
  cell_type_0 <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1), ncol = 3, byrow = T)
  cell_type_1 <- matrix(c(0, 0, 1, 0, 0, 1, 1, 1, 1), ncol = 3, byrow = T)
  cell_type_2 <- matrix(c(0, 0, 1, 0, 0, 1, 0, 0, 1), ncol = 3, byrow = T)
  cell_type_3 <- matrix(c(0, 0, 0, 0, 0, 0, 1, 1, 1), ncol = 3, byrow = T)
  cell_type_4 <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 1), ncol = 3, byrow = T)

  nu_row <- nrow(board)
  nu_cow <- ncol(board)
  walked <- conectores(start_coord_x, start_coord_y, board)
  board_frames <- list()


  for (i in 1:nrow(walked)) {
    print(i / nrow(walked))

    if (board[walked$X1[i], walked$X2[i]] == 0) {
      board[walked$X1[i], walked$X2[i]] <- walked$cell[i]
    } else if (board[walked$X1[i], walked$X2[i]] == 4) {
      next
    } else if (board[walked$X1[i], walked$X2[i]] == 2 & walked$cell[i] == 3) {
      board[walked$X1[i], walked$X2[i]] <- 4
    } else if (board[walked$X1[i], walked$X2[i]] == 3 & walked$cell[i] == 2) {
      board[walked$X1[i], walked$X2[i]] <- 4
    } else if (board[walked$X1[i], walked$X2[i]] == 1) {
      board[walked$X1[i], walked$X2[i]] <- walked$cell[i]
    }


    board_2 <- as.list(board)
    dim(board_2) <- c(nrow(board), ncol(board))

    board_2[, ][board_2[, ] %in% 0] <- list(cell_type_0)
    board_2[, ][board_2[, ] %in% 1] <- list(cell_type_1)
    board_2[, ][board_2[, ] %in% 2] <- list(cell_type_2)
    board_2[, ][board_2[, ] %in% 3] <- list(cell_type_3)
    board_2[, ][board_2[, ] %in% 4] <- list(cell_type_4)

    b <- do.call(cbind, board_2[1, 1:ncol(board)])

    for (k in 2:nrow(board)) {
      a <- do.call(cbind, board_2[k, 1:ncol(board)])
      b <- rbind(b, a)
    }

    b <- cbind(1, b)
    b <- rbind(1, b)
    b[b == 0] <- 2
    board_frames[[i]] <- b
  }

  board_frames <- board_frames[unlist(lapply(board_frames, length) != 0)]
  board_frames <- lapply(board_frames, t)

  return(board_frames[-1])
}

# Generate Maze ----------------------------------------------------------------------------------------------
# SETUP
board <- matrix(0, 50, 50) # 50x50 Board
board_cells <- labyrinth(10, 10, board = board) # Rat starting position (10,10)
board_cells <- esticar(board_cells, 5)
imagem <- board_cells %>% as.cimg()
imagem %>% play(loop = T)

# Saving the maze

f <- tempfile(fileext = ".png", tmpdir = getwd())
save.image(imagem, f, 1)

# Animation ---------------------------------------------------------------------------------------------------
# SETUP

board <- matrix(0, 20, 20) # 20x20 Board
board_frames <- labyrinth_2(1,1, board = board) #  Rat starting position (1,1)
animation <- lapply(board_frames, esticar, 5)
animation <- array(data = unlist(animation), dim = c(dim(animation[[1]]), length(animation), 1)) %>% as.cimg()
animation %>% play(loop = T, delay = 30) 

# Saving the animation

f <- tempfile(fileext=".mp4",tmpdir = "WHERE TO SAVE" )
save.video(animation,f, fps = 30)

