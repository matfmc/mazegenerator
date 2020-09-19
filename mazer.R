library(imager)

neighbors <- function(x, board_size = board){
  
  x <- matrix( c(x[1]+1,x[2],x[1]-1,x[2],x[1],x[2]-1 ,x[1],x[2]+1), ncol = 2, byrow = T)
  x <- x[(x[,1] != 0 & x[,1] <= nrow(board_size) ) & (x[,2] != 0 & x[,2] <= ncol(board_size) ), ]
  x <- split(x, seq(nrow(x)))
  return( x )
  
}


walk <- function(posichon, stack, visited){
  
  possible_neighbors <-  neighbors(posichon)[( neighbors(posichon) %in% visited) == FALSE]
  if( length(possible_neighbors) != 0){
    next_step <- c(unlist(sample(possible_neighbors, 1),use.names = F))
    stack <- append(list(next_step),stack)
    visited <- append(list(next_step),visited)
  } else{ 
    next_step <- stack[-1][[1]]
    stack <- stack[-1]
  }

  return(list(next_step = next_step, stack=stack, visited=visited) )
  
}

mazefy <- function(start_coord_x,start_coord_y, board = board){
  board <- board
  rat <- c(start_coord_x,start_coord_y)
  rat_trail <- list()
  stack <- list()
  visited <- list()
  rat_trail[[1]] <- rat
  stack[[1]] <- rat
  visited[[1]] <- rat
  
  while(length(visited) < nrow(board)*ncol(board) ){
    
    next_step <- walk(rat,stack,visited)
    stack <- next_step$stack
    visited <-  next_step$visited
    rat <- next_step$next_step
    rat_trail <- append(list(rat), rat_trail)
    print(rat)
    print(length(visited))
    
  }
  
  return(rat_trail=rat_trail)
  
}


conectores <- function(start_coord_x = start_coord_x,start_coord_y = start_coord_y, board = board){
  
  board <- board
  walked <- mazefy(start_coord_x = start_coord_x,start_coord_y = start_coord_y, board )
  walked <- walked[length(walked):1]
  walked <- data.frame(t(do.call(data.frame,walked)),row.names = NULL)
  
  for(i in 1:nrow(walked)){
    
    if(i == 1){
     
       walked[i,3:4]<- (walked[i,1:2] - walked[i+1,1:2])
      
    }else if(i == nrow(walked)){
     
       walked[i,3:4]<- (walked[i,1:2] - walked[i-1,1:2])
      
    } else{
      
      walked[i,3:4]<- (walked[i,1:2] - walked[i+1,1:2])
      walked[i,5:6]<- (walked[i,1:2] - walked[i-1,1:2])
      
    }
  }
  
  walked[walked[,3] == -1 & walked[,4] == 0,"conections"] <- "A"
  walked[walked[,3] == 0 & walked[,4] == -1,"conections"] <- "B"
  walked[walked[,3] == 1 & walked[,4] == 0,"conections"] <- "C"
  walked[walked[,3] == 0 & walked[,4] == 1,"conections"] <- "D"
  
  walked[(walked[,5] == -1 & walked[,6] == 0) & !is.na(walked[,5]),"conections2"] <- "A"
  walked[(walked[,5] == 0 & walked[,6] == -1) & !is.na(walked[,5]),"conections2"] <- "B"
  walked[(walked[,5] == 1 & walked[,6] == 0) & !is.na(walked[,5]),"conections2"] <- "C"
  walked[(walked[,5] == 0 & walked[,6] == 1) & !is.na(walked[,5]),"conections2"] <- "D"
  
  walked$conec <- paste0(walked$conections,walked$conections2)
  walked <- walked[,c(1,2,9)]
  
  walked[walked$conec == "CD" | walked$conec == "DC" | walked$conec == "DNA" | walked$conec == "CNA" | walked$conec == "CC"|walked$conec == "DD"  ,"cell"] <- 1 
  walked[walked$conec == "AC" | walked$conec == "CA" | walked$conec == "AD" | walked$conec == "DA" | walked$conec == "ANA" | walked$conec == "AA","cell"] <- 2
  walked[walked$conec == "BC" | walked$conec == "CB" | walked$conec == "BD" | walked$conec == "DB" | walked$conec == "BNA" | walked$conec == "BB","cell"] <- 3
  walked[walked$conec == "AB" | walked$conec == "BA" ,"cell"] <- 4
  
  return(walked)
}


labyrinth <- function(start_coord_x = start_coord_x, start_coord_y = start_coord_y, nuRow, nuCow){

  nuRow <- nrow(board)
  nuCow <- ncol(board)
  walked <- conectores(start_coord_x = start_coord_x,start_coord_y = start_coord_y, board  )
  
  for(i in 1:nrow(walked)){

    if( board[walked$X1[i],walked$X2[i]] == 0 ){
      
      board[walked$X1[i],walked$X2[i]] <- walked$cell[i]
      
    }else if( board[walked$X1[i],walked$X2[i]] == 4 ){
      
      next
      
    }else if( board[walked$X1[i],walked$X2[i]] == 2 & walked$cell[i] == 3   ){
      
      board[walked$X1[i],walked$X2[i]] <- 4 
      
    }else if( board[walked$X1[i],walked$X2[i]] == 3 & walked$cell[i] == 2 ){
      
      board[walked$X1[i],walked$X2[i]] <- 4 
      
    }else if( board[walked$X1[i],walked$X2[i]] == 1 ){
      
      board[walked$X1[i],walked$X2[i]] <- walked$cell[i]
      
    }
    
  }
  

  cell_type0 <- matrix(c(0,0,0,0,0,0,0,0,0), ncol = 3,byrow = T)
  cell_type1 <- matrix(c(0,0,1,0,0,1,1,1,1), ncol = 3,byrow = T)
  cell_type2 <- matrix(c(0,0,1,0,0,1,0,0,1), ncol = 3,byrow = T)
  cell_type3 <- matrix(c(0,0,0,0,0,0,1,1,1), ncol = 3,byrow = T)
  cell_type4 <- matrix(c(0,0,0,0,0,0,0,0,1), ncol = 3,byrow = T)

  board2 <- as.list(board)
  dim(board2) <- c(nrow(board),ncol(board))
  
  board2[,][ board2[,] %in% 0 ] <- list(cell_type0)
  board2[,][ board2[,] %in% 1 ] <- list(cell_type1) 
  board2[,][ board2[,] %in% 2 ] <- list(cell_type2) 
  board2[,][ board2[,] %in% 3 ] <- list(cell_type3) 
  board2[,][ board2[,] %in% 4 ] <- list(cell_type4) 
  
  
  
  b <- do.call(cbind, board2[1,1:nuCow])
  for(i in 2:nuRow){
    a <- do.call(cbind, board2[i,1:nuCow])
    b <- rbind(b,a)
  }
  
  b <- cbind(1,b)
  b <- rbind(1,b)
  b[ b == 0 ] <- 2
  b <- t(b)
  return(b)
  
}

esticar <- function(matrix, multiplicador){
  
  return(matrix[rep(1:nrow(matrix), each = multiplicador), rep(1:ncol(matrix), each = multiplicador)])
  
}


labyrinth2 <- function(start_coord_x = start_coord_x, start_coord_y = start_coord_y, nuRow, nuCow){
  
  cell_type0 <- matrix(c(1,1,1,1,1,1,1,1,1), ncol = 3,byrow = T)
  cell_type1 <- matrix(c(0,0,1,0,0,1,1,1,1), ncol = 3,byrow = T)
  cell_type2 <- matrix(c(0,0,1,0,0,1,0,0,1), ncol = 3,byrow = T)
  cell_type3 <- matrix(c(0,0,0,0,0,0,1,1,1), ncol = 3,byrow = T)
  cell_type4 <- matrix(c(0,0,0,0,0,0,0,0,1), ncol = 3,byrow = T)

  nuRow <- nrow(board)
  nuCow <- ncol(board)
  walked <- conectores(start_coord_x ,start_coord_y , board  )
  board_frames <- list()

  
  for(i in 1:nrow(walked)){
   
    print(i/nrow(walked))
    
    if(     board[walked$X1[i],walked$X2[i]] == 0     ){
      
      board[walked$X1[i],walked$X2[i]] <- walked$cell[i]
      
    }else if( board[walked$X1[i],walked$X2[i]] == 4 ){
      
      next
      
    }else if( board[walked$X1[i],walked$X2[i]] == 2 & walked$cell[i] == 3   ){
      
      board[walked$X1[i],walked$X2[i]] <- 4 
      
    }else if( board[walked$X1[i],walked$X2[i]] == 3 & walked$cell[i] == 2 ){
      
      board[walked$X1[i],walked$X2[i]] <- 4 
      
    }else if( board[walked$X1[i],walked$X2[i]] == 1 ){
      
      board[walked$X1[i],walked$X2[i]] <- walked$cell[i]
      
    }
    
  
    board2 <- as.list(board)
    dim(board2) <- c(nrow(board),ncol(board))
    
    board2[,][ board2[,] %in% 0 ] <- list(cell_type0) 
    board2[,][ board2[,] %in% 1 ] <- list(cell_type1) 
    board2[,][ board2[,] %in% 2 ] <- list(cell_type2) 
    board2[,][ board2[,] %in% 3 ] <- list(cell_type3) 
    board2[,][ board2[,] %in% 4 ] <- list(cell_type4) 
    
    b <- do.call(cbind, board2[1,1:ncol(board)])
    
    for(k in 2:nrow(board)){
      
      a <- do.call(cbind, board2[k,1:ncol(board)])
      b <- rbind(b,a)
      
    }
 
    b <- cbind(1,b)
    b <- rbind(1,b)
    b[ b == 0 ] <- 2
    board_frames[[i]] <- b
    
  }
  
  board_frames <- board_frames[unlist(lapply(board_frames, length) != 0)]
  board_frames <- lapply(board_frames, t)
  
  return(board_frames[-1])
  
}

# 1 - Generate Maze
### SETUP 

a <- 50 # Size of board (axa)
b <- a
board <- matrix(0,a,b)
board_cells <- labyrinth(start_coord_x = 10,start_coord_y = 10) # ( start_coord_x and start_coord_y ) Rat starting position

### !!!
### !!! function "esticar" use with caution!!! Stretches cells by fac, # becomes # # , # # becomes # # # #
#                                                                                # #   # #         # # # #
#                                                                                                  # # # #   
#  recommended for small mazes < 50x50                                                             # # # #  
fac <- 5
board_cells <- esticar(board_cells,fac)
imagem <- board_cells %>% as.cimg()
imagem %>% play(loop = T)

## Saving the maze
#
#f <- tempfile(fileext=".jpeg",tmpdir = "WHERE TO SAVE" ) 
#save.image(imagem,f, 1)
###


# 2 - Animation
### SETUP 

a <- 20 # Size of board (axa)
b <- a
board <- matrix(0,a,b)

board_frames <- labyrinth2(start_coord_x = 1,start_coord_y = 1) # ( start_coord_x and start_coord_y ) Rat starting position


### !!!
### !!! function "esticar" use with caution!!! Stretches cells by fac, # becomes # # , # # becomes # # # #
#                                                                                # #   # #         # # # #
#                                                                                                  # # # #   
#  recommended for small mazes < 25x25                                                             # # # #  
fac <- 5
animation <- lapply(board_frames, esticar,fac)
animation <- array(data = unlist(animation) , dim = c(dim(animation[[1]]),length(animation),1)) %>% as.cimg()
animation  %>% play(loop = T,delay = 30) # play the animation

## Saving the animation
#f <- tempfile(fileext=".mp4",tmpdir = "WHERE TO SAVE" ) 
#save.video(animation,f, fps = 30)
####
