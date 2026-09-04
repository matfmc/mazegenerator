#' Generate a random maze
#'
#' Uses randomized depth-first search with backtracking to generate a perfect
#' rectangular maze: every cell is reachable and there is exactly one simple
#' path between any two cells.
#'
#' @param rows Number of rows in the maze.
#' @param cols Number of columns in the maze.
#' @param start Integer vector `c(row, column)` giving the starting cell.
#' @param seed Optional integer seed for reproducible generation.
#' @param verbose Show generation progress in the console.
#'
#' @return An object of class `maze` containing its dimensions, traversal,
#'   passages, and start and end cells.
#' @export
#'
#' @examples
#' maze <- generate_maze(10, 10, seed = 42)
#' maze
generate_maze <- function(rows = 20L, cols = rows, start = c(1L, 1L),
                          seed = NULL, verbose = FALSE) {
  rows <- .positive_integer(rows, "rows")
  cols <- .positive_integer(cols, "cols")
  start <- .validate_position(start, rows, cols, "start")

  if (!is.null(seed)) {
    seed <- .whole_number(seed, "seed")
    set.seed(seed)
  }

  visited <- matrix(FALSE, nrow = rows, ncol = cols)
  visited[start[1L], start[2L]] <- TRUE
  visited_count <- 1L

  current <- start
  stack <- list(start)
  traversal <- list(start)
  visitation_order <- list(start)
  total_cells <- rows * cols

  while (visited_count < total_cells) {
    candidates <- .cell_neighbors(current, rows, cols)
    keep <- !visited[cbind(candidates[, 1L], candidates[, 2L])]
    candidates <- candidates[keep, , drop = FALSE]

    if (nrow(candidates) > 0L) {
      current <- candidates[sample.int(nrow(candidates), 1L), ]
      current <- as.integer(current)
      stack[[length(stack) + 1L]] <- current
      visited[current[1L], current[2L]] <- TRUE
      visited_count <- visited_count + 1L
      visitation_order[[length(visitation_order) + 1L]] <- current
    } else {
      stack <- stack[-length(stack)]
      current <- stack[[length(stack)]]
    }

    traversal[[length(traversal) + 1L]] <- current

    if (isTRUE(verbose)) {
      cat(sprintf("\rGenerating maze: %5.1f%%", 100 * visited_count / total_cells))
    }
  }

  if (isTRUE(verbose)) {
    cat("\nMaze generation complete.\n")
  }

  structure(
    list(
      rows = rows,
      cols = cols,
      start = start,
      end = as.integer(utils::tail(visitation_order, 1L)[[1L]]),
      traversal = traversal,
      visitation_order = visitation_order,
      passages = .path_edges(traversal),
      seed = seed
    ),
    class = "maze"
  )
}

#' @export
print.maze <- function(x, ...) {
  cat(sprintf("<maze> %d x %d\n", x$rows, x$cols))
  cat(sprintf("  start: (%d, %d)\n", x$start[1L], x$start[2L]))
  cat(sprintf("  end:   (%d, %d)\n", x$end[1L], x$end[2L]))
  cat(sprintf("  cells: %d\n", x$rows * x$cols))
  invisible(x)
}

.cell_neighbors <- function(position, rows, cols) {
  directions <- matrix(
    c(-1L, 0L, 1L, 0L, 0L, -1L, 0L, 1L),
    ncol = 2L,
    byrow = TRUE
  )
  cells <- sweep(directions, 2L, position, `+`)
  cells[
    cells[, 1L] >= 1L & cells[, 1L] <= rows &
      cells[, 2L] >= 1L & cells[, 2L] <= cols,
    ,
    drop = FALSE
  ]
}

.cell_key <- function(position) {
  paste(position[1L], position[2L], sep = ",")
}

.edge_key <- function(from, to) {
  paste(sort(c(.cell_key(from), .cell_key(to))), collapse = "|")
}

.path_edges <- function(path) {
  if (length(path) < 2L) {
    return(character())
  }
  unique(vapply(
    seq_len(length(path) - 1L),
    function(i) .edge_key(path[[i]], path[[i + 1L]]),
    character(1L)
  ))
}

.is_maze <- function(x) {
  inherits(x, "maze") &&
    is.numeric(x$rows) && is.numeric(x$cols) &&
    is.list(x$traversal) && is.character(x$passages)
}

.whole_number <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) || x != floor(x)) {
    stop(sprintf("`%s` must be one whole number.", name), call. = FALSE)
  }
  as.integer(x)
}

.positive_integer <- function(x, name) {
  value <- .whole_number(x, name)
  if (value < 1L) {
    stop(sprintf("`%s` must be greater than zero.", name), call. = FALSE)
  }
  value
}

.validate_position <- function(x, rows, cols, name) {
  if (length(x) != 2L || !is.numeric(x) || anyNA(x) ||
      any(!is.finite(x)) || any(x != floor(x))) {
    stop(sprintf("`%s` must be `c(row, column)` using whole numbers.", name), call. = FALSE)
  }
  x <- as.integer(x)
  if (x[1L] < 1L || x[1L] > rows || x[2L] < 1L || x[2L] > cols) {
    stop(sprintf("`%s` must be inside the maze boundaries.", name), call. = FALSE)
  }
  x
}
