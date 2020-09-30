############################
PossibleNeighbors <- function(x, board) {
  
  x <- matrix(c(x[1] - 1, x[2] - 1, x[1] - 1, x[2], x[1] + 1, x[2] + 1, x[1], x[2] - 1, x[1], x[2] + 1, x[1] + 1, x[2] - 1, x[1] + 1, x[2], x[1] + 1, x[2] + 1), ncol = 2, byrow = T)
  x <- x[diag(board[x[, 1], x[, 2]]) == 1, ]
  
  if( is.null(nrow(x)) ){
    x <- list(x)
  } else {
    x <- split(x, seq(nrow(x)))
  }
  x <- unique(x)
  
  return(x)
}

initializeNodes <- function(board, inicio, destino) {
  
  nodes <- expand.grid(x = c(1:nrow(board)), y = c(1:ncol(board)))
  nodes$Local <- Inf
  nodes$Heuristic <- floor(sqrt((destino[1] - nodes$x)^2 + (destino[2] - nodes$y)^2))
  nodes$Global <- Inf
  nodes$Parent <- NA
  nodes$Visited <- 0
  NodeToTest <- which((nodes$x == inicio[1] & nodes$y == inicio[2]), arr.ind = T)
  nodes[NodeToTest, ]$Local <- 0
  nodes[NodeToTest, ]$Visited <- 1
  nodes[NodeToTest, ]$Global <- nodes[NodeToTest, ]$Local + nodes[NodeToTest, ]$Heuristic

  return(nodes)
}

makepath <- function(nodes, start, destination, nodelist = list()) {
  startx <- start[1]
  starty <- start[2]

  nodelist[[nodes[nodes$x == startx & nodes$y == starty, "Parent"]]] <- nodes[nodes$x == startx & nodes$y == starty, ]
  nextOne <- as.numeric(unlist(strsplit(x = nodes[nodes$x == startx & nodes$y == starty, "Parent"], split = ",")))

  if (all(nextOne == destination)) {
    return(nodelist)
  }

  return(makepath(nodes = nodes, start = nextOne, destination, nodelist))
}

aStar <- function(board, inicio, destino) {
  
  nodes <- initializeNodes(board = board, inicio = inicio, destino = destino)
  nodesToTest <- nodes[nodes$x == inicio[1] & nodes$y == inicio[2], ]
  Node <- nodesToTest

  while (nrow(nodesToTest) != 0) {
  
    neighborhood <- PossibleNeighbors(x = c(Node$x, Node$y), board = board)
    
    for (i in 1:length(neighborhood)) {

      nodesToTest <- rbind(nodesToTest, nodes[nodes$x == neighborhood[[i]][1] & nodes$y == neighborhood[[i]][2], ])

      if (Node$Local + 1 < nodes[nodes$x == neighborhood[[i]][1] & nodes$y == neighborhood[[i]][2], ]$Local) {
        
        nodes[nodes$x == neighborhood[[i]][1] & nodes$y == neighborhood[[i]][2], ]$Parent <- paste(Node$x, Node$y, sep = ",")
        nodes[nodes$x == neighborhood[[i]][1] & nodes$y == neighborhood[[i]][2], ]$Local <- Node$Local + 1
        nodes[nodes$x == neighborhood[[i]][1] & nodes$y == neighborhood[[i]][2], ]$Global <- Node$Local + 1 + nodes[nodes$x == neighborhood[[i]][1] & nodes$y == neighborhood[[i]][2], ]$Heuristic
        nodesToTest[nodesToTest$x == neighborhood[[i]][1] & nodesToTest$y == neighborhood[[i]][2], ] <- nodes[nodes$x == neighborhood[[i]][1] & nodes$y == neighborhood[[i]][2], ]
      
      }
    }

    nodes[(nodes$x == Node$x & nodes$y == Node$y), ]$Visited <- 1
    nodesToTest <- nodesToTest[!(nodesToTest$x == Node$x & nodesToTest$y == Node$y), ]
    nodesToTest <- nodesToTest[order(nodesToTest$Global), ]
    nodesToTest <- nodesToTest[nodesToTest$Visited == 0, ]
    nodesToTest <- nodesToTest[!(nodesToTest$x == destino[1] & nodesToTest$y == destino[2]), ]
    Node <- nodesToTest[1, ]
    print(Node)
  }

  path <- makepath(nodes = nodes, start = destino, destination = inicio) # THESE PARAMETERS ARE CORRECT !
  path <- do.call(rbind, path)
  path <- path[, 1:2]
  path <- rbind(path, data.frame(x = inicio[1], y = inicio[2]))
  row.names(path) <- NULL

  return(path)
}

red_ch <- function(x) { 
  x[x == "S"] <- 0
  x[x == "W"] <- 0
  x[x == "P"] <- 255
  return(x)  
}

grn_ch <- function(x) { 
  x[x == "S"] <- 0
  x[x == "W"] <- 0
  x[x == "P"] <- 255
  return(x)  
}

blu_ch <- function(x) { 
  x[x == "S"] <- 255
  x[x == "W"] <- 0
  x[x == "P"] <- 255
  return(x)  
}


mk_rgb_array <- function(board_frames){
  boardR   <- as.numeric(red_ch(board_frames))
  boardG   <- as.numeric(grn_ch(board_frames))
  boardB   <- as.numeric(blu_ch(board_frames))
  boardRGB <- list(boardR,boardG,boardB)
  return(boardRGB)
}


