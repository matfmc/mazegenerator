.path_cost <- function(solution) {
  total <- 0
  for (i in seq_len(nrow(solution) - 1L)) {
    from <- as.integer(solution[i, ])
    to <- as.integer(solution[i + 1L, ])
    total <- total + if (all(abs(from - to) == 1L)) sqrt(2) else 1
  }
  total
}

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

# ---- Randomized differential testing against BFS (mazes) --------------------
#
# This is the test that actually catches a broken stopping criterion: bfs is
# a trusted, structurally different implementation (a plain FIFO queue, no
# notion of `mu`), so if bidirectional_dijkstra/bidirectional_astar disagree
# with it on path length, or return an invalid path, something is wrong with
# the bidirectional termination or pruning logic, not just with an edge case.

test_that("bidirectional_dijkstra and bidirectional_astar match bfs path length across many random mazes", {
  gen_algorithms <- c("dfs", "prim", "kruskal", "wilson", "hunt_kill", "growing_tree")
  cases <- 0L
  for (seed in 1:60) {
    set.seed(seed)
    rows <- sample(6:22, 1)
    cols <- sample(6:22, 1)
    open_walls <- sample(c(0, 0.15, 0.3), 1)
    galgo <- sample(gen_algorithms, 1)
    maze <- generate_maze(rows, cols, seed = seed, open_walls = open_walls, algorithm = galgo)

    bfs_path <- solve_maze(maze, algorithm = "bfs")
    dijkstra_path <- solve_maze(maze, algorithm = "bidirectional_dijkstra")
    astar_path <- solve_maze(maze, algorithm = "bidirectional_astar")

    expect_equal(nrow(dijkstra_path), nrow(bfs_path), info = sprintf("seed %d (%s)", seed, galgo))
    expect_equal(nrow(astar_path), nrow(bfs_path), info = sprintf("seed %d (%s)", seed, galgo))
    expect_true(.is_valid_path(dijkstra_path, maze), info = sprintf("seed %d (%s)", seed, galgo))
    expect_true(.is_valid_path(astar_path, maze), info = sprintf("seed %d (%s)", seed, galgo))
    cases <- cases + 1L
  }
  expect_gte(cases, 50L)
})

test_that("bidirectional search handles start equal to end", {
  maze <- generate_maze(6, 6, seed = 2)
  for (algo in c("bidirectional_dijkstra", "bidirectional_astar")) {
    path <- solve_maze(maze, start = maze$start, end = maze$start, algorithm = algo)
    expect_equal(nrow(path), 1L)
    expect_equal(as.integer(path[1, ]), maze$start)
  }
})

# ---- Randomized differential testing against Dijkstra (matrix, non-uniform) -
#
# Cell-count parity isn't enough with non-uniform step costs (diagonal moves
# cost sqrt(2), orthogonal ones cost 1): two paths with the same length can
# have different total cost. Compare total path cost against `dijkstra`
# instead.

test_that("bidirectional_dijkstra and bidirectional_astar match dijkstra path cost with non-uniform diagonal costs", {
  set.seed(123)
  cases <- 0L
  for (i in 1:40) {
    rows <- sample(5:16, 1)
    cols <- sample(5:16, 1)
    board <- matrix(sample(c(1, 1, 1, 0), rows * cols, replace = TRUE), nrow = rows)
    board[1, 1] <- 1
    board[rows, cols] <- 1

    reference <- tryCatch(
      solve_maze(board, c(1, 1), c(rows, cols), diagonal = TRUE, algorithm = "dijkstra"),
      error = function(e) NULL
    )
    if (is.null(reference)) next
    reference_cost <- .path_cost(reference)

    dijkstra_path <- solve_maze(board, c(1, 1), c(rows, cols), diagonal = TRUE,
                                algorithm = "bidirectional_dijkstra")
    astar_path <- solve_maze(board, c(1, 1), c(rows, cols), diagonal = TRUE,
                             algorithm = "bidirectional_astar")

    expect_equal(.path_cost(dijkstra_path), reference_cost, info = paste("case", i))
    expect_equal(.path_cost(astar_path), reference_cost, info = paste("case", i))
    cases <- cases + 1L
  }
  expect_gte(cases, 30L)
})

test_that("bidirectional_dijkstra gets the case a naive first-meeting rule gets wrong", {
  # A matrix found by random search specifically because a *naive*
  # bidirectional Dijkstra -- one that stops as soon as the two frontiers
  # first share a visited cell, instead of tracking `mu` (the best complete
  # path found so far) and continuing until neither frontier can improve on
  # it -- returns a path of cost 4 + 2 * sqrt(2) = 6.828427 here, while the
  # true shortest path costs 2 + 3 * sqrt(2) = 6.242641. If
  # `.bidirectional_dijkstra()` or `.nba_star()` ever regress to the naive
  # stopping rule, this test will start failing.
  board <- structure(
    c(1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    dim = c(4L, 6L)
  )
  start <- c(1, 1)
  end <- c(4, 6)
  true_cost_expected <- 2 + 3 * sqrt(2)
  naive_wrong_cost <- 4 + 2 * sqrt(2)

  true_cost <- .path_cost(solve_maze(board, start, end, diagonal = TRUE, algorithm = "dijkstra"))
  expect_equal(true_cost, true_cost_expected, tolerance = 1e-6)

  dijkstra_cost <- .path_cost(
    solve_maze(board, start, end, diagonal = TRUE, algorithm = "bidirectional_dijkstra")
  )
  astar_cost <- .path_cost(
    solve_maze(board, start, end, diagonal = TRUE, algorithm = "bidirectional_astar")
  )

  expect_equal(dijkstra_cost, true_cost_expected, tolerance = 1e-6)
  expect_equal(astar_cost, true_cost_expected, tolerance = 1e-6)
  # The naive (first-meeting) rule would have returned 4 + 2 * sqrt(2) here;
  # make sure that specific wrong value is not what we got.
  expect_false(isTRUE(all.equal(dijkstra_cost, naive_wrong_cost)))
})

test_that("bidirectional_dijkstra errors on impossible routes", {
  blocked <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_error(
    solve_maze(blocked, c(1, 1), c(2, 2), algorithm = "bidirectional_dijkstra"),
    "No path"
  )
  expect_error(
    solve_maze(blocked, c(1, 1), c(2, 2), algorithm = "bidirectional_astar"),
    "No path"
  )
})

# ---- NBA* expansion count vs. plain astar -----------------------------------

test_that("NBA* (bidirectional_astar) does not expand dramatically more than astar on average", {
  set.seed(9)
  astar_steps <- numeric(0)
  nba_steps <- numeric(0)
  for (i in 1:30) {
    rows <- sample(10:30, 1)
    cols <- sample(10:30, 1)
    maze <- generate_maze(rows, cols, seed = i)
    astar_solution <- solve_maze(maze, algorithm = "astar", trace = TRUE)
    nba_solution <- solve_maze(maze, algorithm = "bidirectional_astar", trace = TRUE)
    astar_steps[i] <- length(attr(astar_solution, "search_trace")$steps)
    nba_steps[i] <- length(attr(nba_solution, "search_trace")$steps)
  }
  # Not a strict per-maze improvement claim (Manhattan is a weak heuristic
  # inside a maze's corridors and bidirectional search does not always win),
  # just that on average it is competitive with plain astar.
  expect_lte(mean(nba_steps), mean(astar_steps) * 1.5)
})

# ---- `side` field --------------------------------------------------------

test_that("side is forward/backward for the three bidirectional algorithms and none for the rest", {
  maze <- generate_maze(10, 10, seed = 9, open_walls = 0.2)

  for (algo in c("bidirectional", "bidirectional_dijkstra", "bidirectional_astar")) {
    solution <- solve_maze(maze, algorithm = algo, trace = TRUE)
    sides <- vapply(attr(solution, "search_trace")$steps, function(step) step$side, character(1))
    expect_true("forward" %in% sides, info = algo)
    expect_true("backward" %in% sides, info = algo)
    expect_true(all(sides %in% c("forward", "backward")), info = algo)
  }

  for (algo in c("astar", "dijkstra", "bfs", "greedy", "wall_follower", "dead_end_filling")) {
    solution <- solve_maze(maze, algorithm = algo, trace = TRUE)
    sides <- vapply(attr(solution, "search_trace")$steps, function(step) step$side, character(1))
    expect_true(all(sides == "none"), info = algo)
  }
})

test_that("a search step read without `side` defaults to \"none\" (backward compatibility)", {
  legacy_step <- list(current = c(1L, 1L), opened = matrix(integer(0), ncol = 2L))
  expect_null(legacy_step$side)
  expect_equal(mazegenerator:::.step_side(legacy_step), "none")
})
