.path_cost <- function(solution) {
  total <- 0
  for (i in seq_len(nrow(solution) - 1L)) {
    from <- as.integer(solution[i, ])
    to <- as.integer(solution[i + 1L, ])
    total <- total + if (all(abs(from - to) == 1L)) sqrt(2) else 1
  }
  total
}

test_that("auto heuristic reproduces manhattan for orthogonal movement", {
  maze <- generate_maze(10, 10, seed = 4)
  auto_path <- solve_maze(maze, algorithm = "astar", heuristic = "auto")
  manhattan_path <- solve_maze(maze, algorithm = "astar", heuristic = "manhattan")
  explicit_default_path <- solve_maze(maze, algorithm = "astar")

  expect_equal(auto_path, manhattan_path)
  expect_equal(auto_path, explicit_default_path)
})

test_that("auto heuristic reproduces euclidean once diagonal = TRUE", {
  board <- matrix(1, nrow = 8, ncol = 8)
  auto_path <- solve_maze(board, c(1, 1), c(8, 8), diagonal = TRUE, algorithm = "astar")
  euclidean_path <- solve_maze(
    board, c(1, 1), c(8, 8), diagonal = TRUE, algorithm = "astar", heuristic = "euclidean"
  )

  expect_equal(.path_cost(auto_path), .path_cost(euclidean_path))
})

test_that("manhattan, euclidean, octile, and zero are all admissible for orthogonal movement", {
  for (seed in c(1L, 7L, 42L)) {
    maze <- generate_maze(10, 10, seed = seed)
    bfs_path <- solve_maze(maze, algorithm = "bfs")
    for (h in c("manhattan", "euclidean", "octile", "zero")) {
      astar_path <- solve_maze(maze, algorithm = "astar", heuristic = h)
      expect_equal(nrow(astar_path), nrow(bfs_path), info = h)
    }
  }
})

test_that("euclidean, octile, and zero are all admissible for diagonal movement", {
  set.seed(11)
  for (i in 1:5) {
    rows <- sample(6:14, 1)
    cols <- sample(6:14, 1)
    board <- matrix(sample(c(1, 1, 1, 0), rows * cols, replace = TRUE), nrow = rows)
    board[1, 1] <- 1
    board[rows, cols] <- 1
    dijkstra_path <- tryCatch(
      solve_maze(board, c(1, 1), c(rows, cols), diagonal = TRUE, algorithm = "dijkstra"),
      error = function(e) NULL
    )
    if (is.null(dijkstra_path)) next
    reference_cost <- .path_cost(dijkstra_path)
    for (h in c("euclidean", "octile", "zero")) {
      astar_path <- solve_maze(
        board, c(1, 1), c(rows, cols), diagonal = TRUE, algorithm = "astar", heuristic = h
      )
      expect_equal(.path_cost(astar_path), reference_cost, info = h)
    }
  }
})

test_that("octile is at least as informed as euclidean (expands no more)", {
  board <- matrix(1, nrow = 20, ncol = 20)
  octile_solution <- solve_maze(
    board, c(1, 1), c(20, 20), diagonal = TRUE, algorithm = "astar",
    heuristic = "octile", trace = TRUE
  )
  euclidean_solution <- solve_maze(
    board, c(1, 1), c(20, 20), diagonal = TRUE, algorithm = "astar",
    heuristic = "euclidean", trace = TRUE
  )
  octile_steps <- length(attr(octile_solution, "search_trace")$steps)
  euclidean_steps <- length(attr(euclidean_solution, "search_trace")$steps)
  expect_lte(octile_steps, euclidean_steps)
})

test_that("manhattan heuristic errors when combined with diagonal = TRUE", {
  board <- matrix(1, nrow = 5, ncol = 5)
  expect_error(
    solve_maze(board, c(1, 1), c(5, 5), diagonal = TRUE, algorithm = "astar", heuristic = "manhattan"),
    "inadmissible"
  )
  expect_error(
    solve_maze(
      board, c(1, 1), c(5, 5), diagonal = TRUE, algorithm = "bidirectional_astar",
      heuristic = "manhattan"
    ),
    "inadmissible"
  )
})

test_that("a custom heuristic function is honoured and must be admissible-shaped", {
  maze <- generate_maze(8, 8, seed = 6)
  bfs_path <- solve_maze(maze, algorithm = "bfs")
  custom <- function(position, target) sum(abs(position - target))
  custom_path <- solve_maze(maze, algorithm = "astar", heuristic = custom)

  expect_equal(nrow(custom_path), nrow(bfs_path))
})

test_that("an invalid custom heuristic function errors clearly", {
  board <- matrix(1, nrow = 5, ncol = 5)

  negative_heuristic <- function(position, target) -1
  expect_error(
    solve_maze(board, c(1, 1), c(5, 5), algorithm = "astar", heuristic = negative_heuristic),
    "finite, non-negative"
  )

  non_scalar_heuristic <- function(position, target) c(1, 2)
  expect_error(
    solve_maze(board, c(1, 1), c(5, 5), algorithm = "astar", heuristic = non_scalar_heuristic),
    "finite, non-negative"
  )

  non_numeric_heuristic <- function(position, target) "far"
  expect_error(
    solve_maze(board, c(1, 1), c(5, 5), algorithm = "astar", heuristic = non_numeric_heuristic),
    "finite, non-negative"
  )

  nan_heuristic <- function(position, target) NaN
  expect_error(
    solve_maze(board, c(1, 1), c(5, 5), algorithm = "astar", heuristic = nan_heuristic),
    "finite, non-negative"
  )
})

test_that("an invalid heuristic name errors", {
  maze <- generate_maze(5, 5, seed = 1)
  expect_error(
    solve_maze(maze, algorithm = "astar", heuristic = "not_a_real_heuristic"),
    "should be one of"
  )
})

test_that("heuristic is ignored (with a warning) for algorithms that don't use one", {
  maze <- generate_maze(8, 8, seed = 2)
  for (algo in c("bfs", "dijkstra", "bidirectional", "bidirectional_dijkstra",
                 "wall_follower", "dead_end_filling")) {
    expect_warning(
      solve_maze(maze, algorithm = algo, heuristic = "manhattan"),
      "not used"
    )
  }
  # Not passing `heuristic` at all must never warn.
  expect_warning(solve_maze(maze, algorithm = "bfs"), NA)
})

test_that("chebyshev is provided but documented as not generally admissible here", {
  # Regression guard: chebyshev must still run and return a valid path (it is
  # allowed, just not guaranteed optimal), on a diagonal matrix where it is
  # the loosest possible bound and can, in principle, overestimate.
  board <- matrix(1, nrow = 10, ncol = 10)
  path <- solve_maze(
    board, c(1, 1), c(10, 10), diagonal = TRUE, algorithm = "astar", heuristic = "chebyshev"
  )
  expect_true(nrow(path) > 0)
  expect_equal(as.integer(path[1, ]), c(1L, 1L))
  expect_equal(as.integer(path[nrow(path), ]), c(10L, 10L))
})
