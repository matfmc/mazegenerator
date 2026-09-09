test_that("generated mazes can be solved from start to end", {
  maze <- generate_maze(8, 8, seed = 99)
  solution <- solve_maze(maze)

  expect_equal(as.integer(solution[1, ]), maze$start)
  expect_equal(as.integer(solution[nrow(solution), ]), maze$end)

  if (nrow(solution) > 1) {
    for (i in seq_len(nrow(solution) - 1L)) {
      from <- as.integer(solution[i, ])
      to <- as.integer(solution[i + 1L, ])
      expect_true(mazegenerator:::.edge_key(from, to) %in% maze$passages)
    }
  }
})

test_that("matrix pathfinding respects walls and boundaries", {
  board <- matrix(
    c(1, 1, 0, 0,
      0, 1, 1, 0,
      0, 0, 1, 1,
      0, 0, 0, 1),
    nrow = 4,
    byrow = TRUE
  )

  solution <- solve_maze(board, c(1, 1), c(4, 4))
  expect_equal(nrow(solution), 7)
  expect_true(all(board[as.matrix(solution)] == 1))

  diagonal_solution <- solve_maze(
    board, c(1, 1), c(4, 4), diagonal = TRUE
  )
  expect_equal(nrow(diagonal_solution), 4)
})

test_that("matrix pathfinding reports invalid or impossible routes", {
  blocked <- matrix(c(1, 0, 0, 1), nrow = 2)
  expect_error(solve_maze(blocked, c(1, 1), c(2, 2)), "No path")
  expect_error(solve_maze(blocked, c(1, 2), c(2, 2)), "walkable")
  expect_error(solve_maze(blocked), "required")
})

test_that("the default algorithm is astar", {
  maze <- generate_maze(8, 8, seed = 99)
  default_solution <- solve_maze(maze)
  explicit_solution <- solve_maze(maze, algorithm = "astar")
  expect_equal(default_solution, explicit_solution)
})
