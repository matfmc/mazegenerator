library(magick)

# Function to find valid neighboring cells in the maze
neighbors <- function(x, board_size = board) {
  # Input validation
  if (!is.numeric(x) || length(x) != 2) {
    stop("Input coordinates must be a numeric vector of length 2")
  }
  if (!is.matrix(board_size)) {
    stop("Board size must be a matrix")
  }
  if (any(x < 1) || x[1] > nrow(board_size) || x[2] > ncol(board_size)) {
    stop("Coordinates must be within board boundaries")
  }
  
  # Define adjacent cell directions (up, down, left, right)
  adjacent_directions <- matrix(
    c( 1,  0,    # up
       -1,  0,    # down
       0, -1,    # left
       0,  1),   # right
    ncol = 2, 
    byrow = TRUE
  )
  
  # Calculate adjacent cell coordinates
  adjacent_cells <- sweep(adjacent_directions, 2, x, "+")
  
  # Filter valid coordinates within board boundaries
  valid_cells <- adjacent_cells[
    (adjacent_cells[, 1] > 0 & adjacent_cells[, 1] <= nrow(board_size)) &
      (adjacent_cells[, 2] > 0 & adjacent_cells[, 2] <= ncol(board_size)),
    , drop = FALSE
  ]
  
  # Convert to list format
  neighbor_list <- split(valid_cells, seq(nrow(valid_cells)))
  
  return(neighbor_list)
}


#-------------------------
# Function to implement the walking algorithm for maze generation
walk <- function(position, stack, visited) {
  # Input validation
  if (!is.numeric(position) || length(position) != 2) {
    stop("Position must be a numeric vector of length 2")
  }
  if (!is.list(stack) || !is.list(visited)) {
    stop("Stack and visited must be lists")
  }
  
  # Get all neighboring cells
  all_neighbors <- neighbors(position)
  
  # Find unvisited neighbors
  unvisited_neighbors <- Filter(
    function(x) !any(sapply(visited, function(v) all(v == x))),
    all_neighbors
  )
  
  if (length(unvisited_neighbors) > 0) {
    # Choose random unvisited neighbor
    next_position <- unlist(sample(unvisited_neighbors, 1), use.names = FALSE)
    
    # Update stack and visited lists
    updated_stack <- c(list(next_position), stack)
    updated_visited <- c(list(next_position), visited)
  } else {
    # Backtrack using stack
    if (length(stack) < 2) {
      stop("Stack is empty, cannot backtrack")
    }
    next_position <- stack[[2]]  # Get second element (first is current position)
    updated_stack <- stack[-1]   # Remove current position from stack
    updated_visited <- visited   # Keep visited list unchanged
  }
  
  # Return updated state
  return(list(
    next_step = next_position,
    stack = updated_stack,
    visited = updated_visited
  ))
}


# Function to generate the maze path
mazefy <- function(start_coord_x, start_coord_y, board = board, verbose = FALSE) {
  # Input validation
  if (!is.numeric(c(start_coord_x, start_coord_y)) || 
      start_coord_x < 1 || start_coord_x > nrow(board) ||
      start_coord_y < 1 || start_coord_y > ncol(board)) {
    stop("Starting coordinates must be valid positions within the board")
  }
  if (!is.matrix(board)) {
    stop("Board must be a matrix")
  }
  
  # Initialize maze generation state
  initial_position <- c(start_coord_x, start_coord_y)
  maze_state <- list(
    current_position = initial_position,
    path_history = list(initial_position),
    stack = list(initial_position),
    visited = list(initial_position)
  )
  
  # Calculate total cells to visit
  total_cells <- nrow(board) * ncol(board)
  
  # Generate maze path
  tryCatch({
    while (length(maze_state$visited) < total_cells) {
      # Get next step using walk function
      next_state <- walk(
        maze_state$current_position,
        maze_state$stack,
        maze_state$visited
      )
      
      # Update maze state
      maze_state$stack <- next_state$stack
      maze_state$visited <- next_state$visited
      maze_state$current_position <- next_state$next_step
      maze_state$path_history <- c(
        list(next_state$next_step),
        maze_state$path_history
      )
      
      # Print progress if verbose is TRUE
      if (verbose) {
        progress <- length(maze_state$visited) / total_cells * 100
        cat(sprintf("\rProgress: %.1f%% - Current position: (%d, %d)",
                    progress,
                    maze_state$current_position[1],
                    maze_state$current_position[2]
        ))
      }
    }
    if (verbose) cat("\nMaze generation complete!\n")
    
    # Reverse the path to match the start and end points
    maze_state$path_history <- rev(maze_state$path_history)
  },
  error = function(e) {
    stop(sprintf("Error during maze generation: %s", e$message))
  })
  
  return(maze_state$path_history)
}

plot_maze <- function(maze_path, board, wall_thickness = 8, path_thickness = 2, 
                      draw_nodes = FALSE, node_size = 0.5, node_color = "blue") {
  # Save current graphics parameters to restore later
  old_par <- par(no.readonly = TRUE)
  
  # Ensure graphics parameters are restored when function exits
  on.exit({
    par(old_par)
    # Force garbage collection to free memory
    gc()
  })
  
  # Input validation
  if (!is.list(maze_path) || length(maze_path) < 1) {
    stop("maze_path must be a non-empty list of coordinates")
  }
  if (!is.matrix(board)) {
    stop("board must be a matrix")
  }
  
  tryCatch({
    # Set up the plot
    par(mar = c(2, 2, 2, 2))  # Adjust margins
    plot.new()
    plot.window(
      xlim = c(0.5, ncol(board) + 0.5), 
      ylim = c(0.5, nrow(board) + 0.5),
      asp = 1  # Keep square aspect ratio
    )
    
    # Convert path coordinates to a matrix
    path_coords <- do.call(rbind, maze_path)
    
    # Create a matrix to track which cells are in the path
    path_matrix <- matrix(FALSE, nrow = nrow(board), ncol = ncol(board))
    for(i in 1:nrow(path_coords)) {
      path_matrix[path_coords[i,1], path_coords[i,2]] <- TRUE
    }
    
    # Fill squares not in the path
    for(i in 1:nrow(board)) {
      for(j in 1:ncol(board)) {
        if(!path_matrix[i,j]) {
          rect(j-0.5, i-0.5, j+0.5, i+0.5, col = "black", border = NA)
        }
      }
    }
    
    # Convert path to segments
    path_segments <- vector("list", nrow(path_coords)-1)  # Pre-allocate list
    for(i in 1:(nrow(path_coords)-1)) {
      path_segments[[i]] <- list(
        x = path_coords[i:(i+1), 2],
        y = path_coords[i:(i+1), 1]
      )
    }
    
    # Draw horizontal walls
    for (i in 0:nrow(board)) {
      y = i + 0.5
      x_start = 0.5
      x_current = x_start
      
      for (j in 1:ncol(board)) {
        should_draw <- TRUE
        for (seg in path_segments) {
          if ((min(seg$y) < y && max(seg$y) > y) &&
              (min(seg$x) <= j + 0.5 && max(seg$x) >= j - 0.5)) {
            should_draw <- FALSE
            break
          }
        }
        
        if (should_draw) {
          segments(
            x0 = j - 0.5, x1 = j + 0.5,
            y0 = y, y1 = y,
            lwd = wall_thickness, col = "black"
          )
        }
      }
    }
    
    # Draw vertical walls
    for (j in 0:ncol(board)) {
      x = j + 0.5
      y_start = 0.5
      y_current = y_start
      
      for (i in 1:nrow(board)) {
        should_draw <- TRUE
        for (seg in path_segments) {
          if ((min(seg$x) < x && max(seg$x) > x) &&
              (min(seg$y) <= i + 0.5 && max(seg$y) >= i - 0.5)) {
            should_draw <- FALSE
            break
          }
        }
        
        if (should_draw) {
          segments(
            x0 = x, x1 = x,
            y0 = i - 0.5, y1 = i + 0.5,
            lwd = wall_thickness, col = "black"
          )
        }
      }
    }
    
    # Draw nodes if requested
    if (draw_nodes) {
      # Draw path nodes
      for (i in 1:length(maze_path)) {
        point <- maze_path[[i]]
        # Skip start and end points as they're drawn differently
        if (i != 1 && i != length(maze_path)) {
          points(
            point[2], point[1],
            col = node_color,
            pch = 19,  # Filled circle
            cex = node_size
          )
          
          # Add node number if desired
          text(
            point[2], point[1],
            labels = i,
            pos = 3,
            offset = 0.3,
            cex = 0.7,
            col = node_color
          )
        }
      }
      
      # Draw path lines between nodes
      if (length(maze_path) > 1) {
        for (i in 1:(length(maze_path)-1)) {
          current <- maze_path[[i]]
          next_point <- maze_path[[i+1]]
          lines(
            x = c(current[2], next_point[2]),
            y = c(current[1], next_point[1]),
            col = adjustcolor(node_color, alpha.f = 0.5),
            lwd = path_thickness
          )
        }
      }
    }
    
    # Mark start and end points
    start_point <- maze_path[[length(maze_path)]]  # Last point becomes start
    end_point <- maze_path[[1]]  # First point becomes end
    
    
    # Draw rat point (green)
    points(
      start_point[2], start_point[1],
      col = "red", pch = 15, cex = 4
    )
    # Add start label
    text(
      start_point[2], start_point[1] - 0.2,
      "Rat", col = "red", pos = 1
    )
    
    # Draw start point (red)
    points(
      end_point[2], end_point[1],
      col = "green", pch = 19, cex = 2
    )
    # Add end label
    text(
      end_point[2], end_point[1] + 0.2,
      "Start", col = "green", pos = 3
    )
    
    # Add title
    title(main = paste0(nrow(board), "x", ncol(board), " Maze"))
    
  }, error = function(e) {
    warning(sprintf("Error in plot_maze: %s", e$message))
    # Ensure the error is propagated
    stop(e)
  }, finally = {
    # Clean up any remaining resources
    if (exists("path_matrix")) rm(path_matrix)
    if (exists("path_segments")) rm(path_segments)
    if (exists("path_coords")) rm(path_coords)
  })
}

create_maze_animation <- function(maze_path, board, output_file = "maze_animation.gif", 
                                  fps = 10, png_width = 800, png_height = 800,
                                  wall_thickness = 8, path_thickness = 2) {
  

  # Create temporary directory for frames (remains the same)
  temp_dir <- tempdir()
  maze_frames_dir <- file.path(temp_dir, "maze_frames")
  dir.create(maze_frames_dir, showWarnings = FALSE)
  on.exit(unlink(maze_frames_dir, recursive = TRUE), add = TRUE) # Important: add = TRUE
  
  total_frames <- length(maze_path)
  
  # Initialize the animation
  filename <- file.path(maze_frames_dir, sprintf("maze_step_%04d.png", 2))
  png(filename, width = png_width, height = png_height)
  tryCatch({
    plot_maze(maze_path[1:2], board, wall_thickness, path_thickness)
  }, error = function(e) {
    message(sprintf("Error at frame 2: %s", e$message))
  })
  dev.off()
  
  first_image <- image_read(filename)
  animation <- image_animate(first_image, fps = fps)
  
  # Generate and append frames iteratively
  pb <- progress::progress_bar$new(total = total_frames - 2,
                                   format = "Appending frames [:bar] :percent eta: :eta")
  for(i in 3:total_frames) {
    filename <- file.path(maze_frames_dir, sprintf("maze_step_%04d.png", i))
    
    png(filename, width = png_width, height = png_height)
    tryCatch({
      plot_maze(maze_path[1:i], board, wall_thickness, path_thickness)
    }, error = function(e) {
      message(sprintf("Error at frame %d: %s", i, e$message))
    })
    dev.off()
    
    animation <- c(animation, image_read(filename))
    pb$tick()
  }
  
  # Save animation (remains the same)
  image_write(animation, output_file)
  message(sprintf("\nAnimation saved as '%s'\n", output_file))
}
