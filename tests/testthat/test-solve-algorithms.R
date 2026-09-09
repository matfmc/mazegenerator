.true_bfs_distances <- function(maze) {
  rows <- maze$rows
  cols <- maze$cols
  start <- maze$start
  dist <- matrix(Inf, nrow = rows, ncol = cols)
  dist[start[1L], start[2L]] <- 0
  queue <- list(start)
  head_index <- 1L
  while (head_index <= length(queue)) {
    current <- queue[[head_index]]
    head_index <- head_index + 1L
    candidates <- mazegenerator:::.cell_neighbors(current, rows, cols)
    if (nrow(candidates) == 0L) next
    for (i in seq_len(nrow(candidates))) {
      candidate <- as.integer(candidates[i, ])
      if (!mazegenerator:::.has_passage(maze, current, candidate)) next
      if (is.finite(dist[candidate[1L], candidate[2L]])) next
      dist[candidate[1L], candidate[2L]] <- dist[current[1L], current[2L]] + 1
      queue[[length(queue) + 1L]] <- candidate
    }
  }
  dist
}

# Returns TRUE when `solution` is a data frame of `c(row, column)` cells that
# starts at `maze$start`, ends at `maze$end`, and only ever steps between
# cells connected by an open passage. It does NOT require the path to be
# simple (non-repeating) or shortest -- algorithms like "greedy" and
# "wall_follower" are not guaranteed to be optimal, so this is the weaker
# guarantee shared by every algorithm.
.is_valid_path <- function(solution, maze) {
  if (!is.data.frame(solution) || !all(c("row", "column") %in% names(solution))) {
    return(FALSE)
  }
  if (nrow(solution) == 0L) {
    return(FALSE)
  }
  if (!identical(as.integer(solution[1L, ]), as.integer(maze$start))) {
    return(FALSE)
  }
  if (!identical(as.integer(solution[nrow(solution), ]), as.integer(maze$end))) {
    return(FALSE)
  }
  if (nrow(solution) > 1L) {
    for (i in seq_len(nrow(solution) - 1L)) {
      from <- as.integer(solution[i, ])
      to <- as.integer(solution[i + 1L, ])
      if (sum(abs(from - to)) != 1L) {
        return(FALSE)
      }
      if (!(mazegenerator:::.edge_key(from, to) %in% maze$passages)) {
        return(FALSE)
      }
    }
  }
  TRUE
}

.expect_valid_maze_path <- function(solution, maze) {
  expect_true(.is_valid_path(solution, maze))
}

test_that("all algorithms agree on path length for perfect mazes", {
  for (seed in c(1L, 7L, 42L)) {
    maze <- generate_maze(10, 10, seed = seed)
    astar_path <- solve_maze(maze, algorithm = "astar")
    dijkstra_path <- solve_maze(maze, algorithm = "dijkstra")
    bfs_path <- solve_maze(maze, algorithm = "bfs")

    expect_equal(nrow(astar_path), nrow(dijkstra_path))
    expect_equal(nrow(astar_path), nrow(bfs_path))
    # In a perfect maze the path is unique, so all three should be identical.
    expect_equal(astar_path, dijkstra_path)
    expect_equal(astar_path, bfs_path)

    .expect_valid_maze_path(astar_path, maze)
    .expect_valid_maze_path(dijkstra_path, maze)
    .expect_valid_maze_path(bfs_path, maze)
  }
})

test_that("all algorithms agree on path length for braided mazes", {
  for (seed in c(3L, 11L)) {
    maze <- generate_maze(12, 12, seed = seed, open_walls = 0.3)
    astar_path <- solve_maze(maze, algorithm = "astar")
    dijkstra_path <- solve_maze(maze, algorithm = "dijkstra")
    bfs_path <- solve_maze(maze, algorithm = "bfs")

    expect_equal(nrow(astar_path), nrow(dijkstra_path))
    expect_equal(nrow(astar_path), nrow(bfs_path))

    .expect_valid_maze_path(astar_path, maze)
    .expect_valid_maze_path(dijkstra_path, maze)
    .expect_valid_maze_path(bfs_path, maze)
  }
})

test_that("A* expands no more cells than Dijkstra on the same maze", {
  maze <- generate_maze(25, 25, seed = 5)
  astar_solution <- solve_maze(maze, algorithm = "astar", trace = TRUE)
  dijkstra_solution <- solve_maze(maze, algorithm = "dijkstra", trace = TRUE)

  astar_steps <- length(attr(astar_solution, "search_trace")$steps)
  dijkstra_steps <- length(attr(dijkstra_solution, "search_trace")$steps)

  expect_lte(astar_steps, dijkstra_steps)
  expect_lt(astar_steps, dijkstra_steps)
})

test_that("BFS expands cells in non-decreasing distance-from-start order", {
  maze <- generate_maze(10, 10, seed = 21)
  true_dist <- .true_bfs_distances(maze)

  solution <- solve_maze(maze, algorithm = "bfs", trace = TRUE)
  trace <- attr(solution, "search_trace")
  expansion_order <- vapply(
    trace$steps,
    function(step) true_dist[step$current[1L], step$current[2L]],
    numeric(1)
  )

  expect_true(all(diff(expansion_order) >= 0))
})

test_that("matrix input works for all algorithms", {
  board <- matrix(
    c(1, 1, 0, 0,
      0, 1, 1, 0,
      0, 0, 1, 1,
      0, 0, 0, 1),
    nrow = 4,
    byrow = TRUE
  )

  astar_solution <- solve_maze(board, c(1, 1), c(4, 4), algorithm = "astar")
  dijkstra_solution <- solve_maze(board, c(1, 1), c(4, 4), algorithm = "dijkstra")
  bfs_solution <- solve_maze(board, c(1, 1), c(4, 4), algorithm = "bfs")

  expect_equal(nrow(astar_solution), 7)
  expect_equal(nrow(dijkstra_solution), 7)
  expect_equal(nrow(bfs_solution), 7)
})

test_that("bfs errors on a matrix with diagonal movement", {
  board <- matrix(1, nrow = 4, ncol = 4)
  expect_error(
    solve_maze(board, c(1, 1), c(4, 4), diagonal = TRUE, algorithm = "bfs"),
    "uniform step costs"
  )
})

test_that("dijkstra and astar agree on path cost with diagonal movement", {
  board <- matrix(1, nrow = 4, ncol = 4)
  astar_solution <- solve_maze(board, c(1, 1), c(4, 4), diagonal = TRUE, algorithm = "astar")
  dijkstra_solution <- solve_maze(board, c(1, 1), c(4, 4), diagonal = TRUE, algorithm = "dijkstra")

  .path_cost <- function(solution) {
    total <- 0
    for (i in seq_len(nrow(solution) - 1L)) {
      from <- as.integer(solution[i, ])
      to <- as.integer(solution[i + 1L, ])
      total <- total + if (all(abs(from - to) == 1L)) sqrt(2) else 1
    }
    total
  }

  expect_equal(.path_cost(astar_solution), .path_cost(dijkstra_solution))
})

test_that("all algorithms report impossible routes on a matrix", {
  blocked <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_error(solve_maze(blocked, c(1, 1), c(2, 2), algorithm = "astar"), "No path")
  expect_error(solve_maze(blocked, c(1, 1), c(2, 2), algorithm = "dijkstra"), "No path")
  expect_error(solve_maze(blocked, c(1, 1), c(2, 2), algorithm = "bfs"), "No path")
})

test_that("an invalid algorithm value errors", {
  # "greedy" used to be the placeholder invalid value here, but it is now a
  # real algorithm (see below), so a genuinely unknown name is used instead.
  maze <- generate_maze(5, 5, seed = 1)
  expect_error(solve_maze(maze, algorithm = "not_a_real_algorithm"), "should be one of")
})

test_that("search trace shape is identical across algorithms", {
  maze <- generate_maze(8, 8, seed = 9)
  algorithms <- c(
    "astar", "dijkstra", "bfs", "greedy", "bidirectional",
    "wall_follower", "dead_end_filling"
  )
  for (algo in algorithms) {
    solution <- solve_maze(maze, algorithm = algo, trace = TRUE)
    trace <- attr(solution, "search_trace")

    expect_s3_class(trace, "maze_search_trace")
    expect_named(trace, c("rows", "cols", "start", "end", "algorithm", "steps"))
    expect_equal(trace$algorithm, algo)
    expect_true(length(trace$steps) > 0)

    for (step in trace$steps) {
      expect_type(step$current, "integer")
      expect_length(step$current, 2L)
      expect_true(is.matrix(step$opened))
      expect_equal(ncol(step$opened), 2L)
    }
  }
})

test_that("greedy finds a valid, but not necessarily shortest, path", {
  for (seed in c(1L, 7L, 42L)) {
    maze <- generate_maze(10, 10, seed = seed)
    bfs_path <- solve_maze(maze, algorithm = "bfs")
    greedy_path <- solve_maze(maze, algorithm = "greedy")

    .expect_valid_maze_path(greedy_path, maze)
    expect_gte(nrow(greedy_path), nrow(bfs_path))
  }
})

test_that("greedy expands no more than dijkstra tends to on a directed search", {
  maze <- generate_maze(20, 20, seed = 5)
  greedy_solution <- solve_maze(maze, algorithm = "greedy", trace = TRUE)
  greedy_steps <- length(attr(greedy_solution, "search_trace")$steps)
  expect_true(greedy_steps > 0)
  .expect_valid_maze_path(greedy_solution, maze)
})

test_that("bidirectional search is optimal like bfs on perfect and braided mazes", {
  for (seed in c(1L, 7L, 42L)) {
    maze <- generate_maze(10, 10, seed = seed)
    bfs_path <- solve_maze(maze, algorithm = "bfs")
    bidirectional_path <- solve_maze(maze, algorithm = "bidirectional")

    expect_equal(nrow(bidirectional_path), nrow(bfs_path))
    .expect_valid_maze_path(bidirectional_path, maze)
  }

  for (seed in c(3L, 11L)) {
    maze <- generate_maze(12, 12, seed = seed, open_walls = 0.3)
    bfs_path <- solve_maze(maze, algorithm = "bfs")
    bidirectional_path <- solve_maze(maze, algorithm = "bidirectional")

    expect_equal(nrow(bidirectional_path), nrow(bfs_path))
    .expect_valid_maze_path(bidirectional_path, maze)
  }
})

test_that("bidirectional search handles start equal to end", {
  maze <- generate_maze(6, 6, seed = 2)
  path <- solve_maze(maze, start = maze$start, end = maze$start, algorithm = "bidirectional")
  expect_equal(nrow(path), 1L)
  expect_equal(as.integer(path[1, ]), maze$start)
})

test_that("bidirectional errors on a matrix with diagonal movement", {
  board <- matrix(1, nrow = 4, ncol = 4)
  expect_error(
    solve_maze(board, c(1, 1), c(4, 4), diagonal = TRUE, algorithm = "bidirectional"),
    "uniform step costs"
  )
})

test_that("wall_follower finds a valid path on perfect mazes with either hand", {
  for (seed in c(1L, 7L, 42L)) {
    maze <- generate_maze(10, 10, seed = seed)
    left_path <- solve_maze(maze, algorithm = "wall_follower", hand = "left")
    right_path <- solve_maze(maze, algorithm = "wall_follower", hand = "right")

    expect_true(nrow(left_path) > 0)
    expect_true(nrow(right_path) > 0)
    .expect_valid_maze_path(left_path, maze)
    .expect_valid_maze_path(right_path, maze)
  }
})

test_that("wall_follower errors on a matrix with diagonal movement", {
  board <- matrix(1, nrow = 4, ncol = 4)
  expect_error(
    solve_maze(board, c(1, 1), c(4, 4), diagonal = TRUE, algorithm = "wall_follower"),
    "orthogonal movement"
  )
})

test_that("wall_follower gives up with a warning on a loop that traps it", {
  # A ring-shaped matrix: the outer ring is walkable and touches a small
  # inner pocket only through single-cell "doors" at (3, 4) and (4, 4). A
  # hand kept on the outer wall surface circles the ring and never steps
  # into the pocket, so the follower cannot reach `end` and must give up.
  board <- matrix(0, nrow = 7, ncol = 7)
  board[2:6, 2:6] <- 1
  board[3:5, 3:5] <- 0
  board[4, 4] <- 1
  board[3, 4] <- 1

  expect_warning(
    path <- solve_maze(board, c(2, 2), c(4, 4), algorithm = "wall_follower"),
    "did not reach `end`"
  )
  expect_equal(nrow(path), 0L)
  expect_named(path, c("row", "column"))
})

test_that("dead_end_filling matches bfs on perfect mazes", {
  for (seed in c(1L, 7L, 42L)) {
    maze <- generate_maze(10, 10, seed = seed)
    bfs_path <- solve_maze(maze, algorithm = "bfs")
    dead_end_path <- solve_maze(maze, algorithm = "dead_end_filling")

    expect_equal(dead_end_path, bfs_path)
    .expect_valid_maze_path(dead_end_path, maze)
  }
})

test_that("dead_end_filling falls back to bfs correctness on braided mazes", {
  for (seed in c(3L, 11L)) {
    maze <- generate_maze(12, 12, seed = seed, open_walls = 0.3)
    bfs_path <- solve_maze(maze, algorithm = "bfs")
    dead_end_path <- solve_maze(maze, algorithm = "dead_end_filling")

    expect_equal(nrow(dead_end_path), nrow(bfs_path))
    .expect_valid_maze_path(dead_end_path, maze)
  }
})

test_that("dead_end_filling trace begins with fill steps with no opened cells", {
  maze <- generate_maze(8, 8, seed = 9)
  solution <- solve_maze(maze, algorithm = "dead_end_filling", trace = TRUE)
  trace <- attr(solution, "search_trace")
  expect_true(length(trace$steps) > 0)
  first_step <- trace$steps[[1L]]
  expect_equal(nrow(first_step$opened), 0L)
})
