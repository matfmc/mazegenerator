#' Solve a maze or walkable matrix with A-star search
#'
#' For a `maze` object, the function follows its passages. A numeric or logical
#' matrix can also be supplied, in which case cells matching `walkable` may be
#' crossed.
#'
#' @param x A `maze` object or a matrix.
#' @param start,end Integer vectors `c(row, column)`. For a maze, they default to
#'   its recorded start and end. They are required for a matrix.
#' @param walkable Values considered walkable when `x` is a matrix.
#' @param diagonal Allow diagonal movement for a matrix. Maze passages are always
#'   orthogonal.
#'
#' @return A data frame with `row` and `column`, ordered from start to end.
#' @export
#'
#' @examples
#' maze <- generate_maze(10, 10, seed = 42)
#' path <- solve_maze(maze)
#' plot_maze(maze, solution = path)
solve_maze <- function(x, start = NULL, end = NULL, walkable = 1,
                       diagonal = FALSE) {
  if (.is_maze(x)) {
    rows <- x$rows
    cols <- x$cols
    if (is.null(start)) start <- x$start
    if (is.null(end)) end <- x$end
    start <- .validate_position(start, rows, cols, "start")
    end <- .validate_position(end, rows, cols, "end")
    neighbor_function <- function(position) {
      candidates <- .cell_neighbors(position, rows, cols)
      keep <- apply(candidates, 1L, function(candidate) {
        .has_passage(x, position, candidate)
      })
      candidates[keep, , drop = FALSE]
    }
    heuristic <- function(position) sum(abs(position - end))
    step_cost <- function(from, to) 1
  } else if (is.matrix(x)) {
    rows <- nrow(x)
    cols <- ncol(x)
    if (is.null(start) || is.null(end)) {
      stop("`start` and `end` are required when `x` is a matrix.", call. = FALSE)
    }
    start <- .validate_position(start, rows, cols, "start")
    end <- .validate_position(end, rows, cols, "end")
    diagonal <- isTRUE(diagonal)

    if (!(x[start[1L], start[2L]] %in% walkable) ||
        !(x[end[1L], end[2L]] %in% walkable)) {
      stop("The start and end cells must both be walkable.", call. = FALSE)
    }

    neighbor_function <- function(position) {
      directions <- matrix(
        c(-1L, 0L, 1L, 0L, 0L, -1L, 0L, 1L),
        ncol = 2L,
        byrow = TRUE
      )
      if (diagonal) {
        directions <- rbind(
          directions,
          c(-1L, -1L), c(-1L, 1L), c(1L, -1L), c(1L, 1L)
        )
      }
      candidates <- sweep(directions, 2L, position, `+`)
      inside <- candidates[, 1L] >= 1L & candidates[, 1L] <= rows &
        candidates[, 2L] >= 1L & candidates[, 2L] <= cols
      candidates <- candidates[inside, , drop = FALSE]
      candidates[x[cbind(candidates[, 1L], candidates[, 2L])] %in% walkable, , drop = FALSE]
    }
    heuristic <- if (diagonal) {
      function(position) sqrt(sum((position - end)^2))
    } else {
      function(position) sum(abs(position - end))
    }
    step_cost <- function(from, to) {
      if (all(abs(from - to) == 1L)) sqrt(2) else 1
    }
  } else {
    stop("`x` must be a maze object or a matrix.", call. = FALSE)
  }

  .a_star(rows, cols, start, end, neighbor_function, heuristic, step_cost)
}

.a_star <- function(rows, cols, start, end, neighbor_function, heuristic,
                    step_cost) {
  g_score <- matrix(Inf, nrow = rows, ncol = cols)
  f_score <- matrix(Inf, nrow = rows, ncol = cols)
  closed <- matrix(FALSE, nrow = rows, ncol = cols)
  parent_row <- matrix(NA_integer_, nrow = rows, ncol = cols)
  parent_col <- matrix(NA_integer_, nrow = rows, ncol = cols)

  g_score[start[1L], start[2L]] <- 0
  f_score[start[1L], start[2L]] <- heuristic(start)
  open <- matrix(start, nrow = 1L, dimnames = list(NULL, c("row", "column")))

  while (nrow(open) > 0L) {
    priorities <- f_score[cbind(open[, 1L], open[, 2L])]
    current_index <- which.min(priorities)
    current <- as.integer(open[current_index, ])
    open <- open[-current_index, , drop = FALSE]

    if (all(current == end)) {
      return(.reconstruct_path(parent_row, parent_col, start, end))
    }
    if (closed[current[1L], current[2L]]) {
      next
    }
    closed[current[1L], current[2L]] <- TRUE

    neighbors <- neighbor_function(current)
    if (nrow(neighbors) == 0L) {
      next
    }

    for (i in seq_len(nrow(neighbors))) {
      neighbor <- as.integer(neighbors[i, ])
      if (closed[neighbor[1L], neighbor[2L]]) {
        next
      }
      tentative <- g_score[current[1L], current[2L]] + step_cost(current, neighbor)
      if (tentative < g_score[neighbor[1L], neighbor[2L]]) {
        parent_row[neighbor[1L], neighbor[2L]] <- current[1L]
        parent_col[neighbor[1L], neighbor[2L]] <- current[2L]
        g_score[neighbor[1L], neighbor[2L]] <- tentative
        f_score[neighbor[1L], neighbor[2L]] <- tentative + heuristic(neighbor)

        already_open <- nrow(open) > 0L && any(
          open[, 1L] == neighbor[1L] & open[, 2L] == neighbor[2L]
        )
        if (!already_open) {
          open <- rbind(open, neighbor)
        }
      }
    }
  }

  stop("No path exists between `start` and `end`.", call. = FALSE)
}

.reconstruct_path <- function(parent_row, parent_col, start, end) {
  path <- list(end)
  current <- end
  while (!all(current == start)) {
    previous <- c(
      parent_row[current[1L], current[2L]],
      parent_col[current[1L], current[2L]]
    )
    if (anyNA(previous)) {
      stop("Could not reconstruct the path.", call. = FALSE)
    }
    current <- as.integer(previous)
    path[[length(path) + 1L]] <- current
  }
  coordinates <- do.call(rbind, rev(path))
  data.frame(row = coordinates[, 1L], column = coordinates[, 2L])
}
