#' Animate a maze search
#'
#' Replays the search recorded by `solve_maze(trace = TRUE)` and writes it to an
#' animated GIF. Each frame shades the cells already expanded, the cells waiting
#' on the frontier, and the cell being expanded, so the characteristic shape of
#' each search becomes visible: A-star's bias towards the goal, Dijkstra's
#' uniform spread, and breadth-first search's expanding rings. The frame titles
#' name whichever algorithm produced the trace. The final frames hold the solved
#' path. The optional `magick` package is required only when this function is
#' called.
#'
#' For a trace produced by one of the three bidirectional algorithms
#' (`"bidirectional"`, `"bidirectional_dijkstra"`, `"bidirectional_astar"`),
#' each step also records which frontier it belongs to (see the `side` field
#' documented in [solve_maze()]), and cells touched by the forward search
#' (growing from `start`) and the backward search (growing from `end`) are
#' shaded with `forward_color` and `backward_color` instead of the usual
#' `explored_color` / `frontier_color` pair. A trace from any other algorithm
#' carries no side information and renders exactly as before.
#'
#' @param maze A `maze` object returned by [generate_maze()].
#' @param solution Optional result of [solve_maze()] called with `trace = TRUE`.
#'   When omitted the maze is solved again with tracing enabled.
#' @param output_file Destination path for the GIF.
#' @param fps Frames per second.
#' @param every Render every nth search step. Increase this for large mazes.
#' @param width,height Frame dimensions in pixels.
#' @param wall_width Width of wall lines.
#' @param explored_color Fill for cells that have been expanded. Used for
#'   every algorithm except that, for a trace carrying `side` information (see
#'   Details), cells touched by either bidirectional frontier are shaded with
#'   `forward_color` / `backward_color` instead.
#' @param frontier_color Fill for cells waiting in the open set. Same caveat
#'   as `explored_color`.
#' @param current_color Fill for the cell being expanded.
#' @param forward_color Fill for cells (explored or still on the frontier)
#'   belonging to the forward search -- the one growing from `start` -- of a
#'   bidirectional trace. Ignored for a trace with no `side` information.
#' @param backward_color Fill for cells belonging to the backward search --
#'   the one growing from `end` -- of a bidirectional trace. Ignored for a
#'   trace with no `side` information.
#' @param solution_color Color of the path drawn in the closing frames.
#' @param hold Number of closing frames showing the solved path.
#' @param progress Show a progress bar in interactive sessions.
#'
#' @return The normalized output path, invisibly.
#' @export
#'
#' @examples
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   maze <- generate_maze(8, 8, seed = 42)
#'   animate_solution(maze, output_file = tempfile(fileext = ".gif"))
#' }
animate_solution <- function(maze, solution = NULL,
                             output_file = "maze-solution.gif", fps = 10,
                             every = 1L, width = 800L, height = 800L,
                             wall_width = 3,
                             explored_color = "#bae6fd",
                             frontier_color = "#fdba74",
                             current_color = "#8b5cf6",
                             forward_color = "#0ea5e9",
                             backward_color = "#f43f5e",
                             solution_color = "#f59e0b",
                             hold = 12L, progress = interactive()) {
  if (!.is_maze(maze)) {
    stop("`maze` must be an object returned by `generate_maze()`.", call. = FALSE)
  }
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package `magick` is required for animation. Install it with `install.packages(\"magick\")`.", call. = FALSE)
  }

  fps <- .positive_integer(fps, "fps")
  every <- .positive_integer(every, "every")
  width <- .positive_integer(width, "width")
  height <- .positive_integer(height, "height")
  hold <- .positive_integer(hold, "hold")

  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    stop("The directory containing `output_file` does not exist.", call. = FALSE)
  }

  if (is.null(solution)) {
    solution <- solve_maze(maze, trace = TRUE)
  }
  trace <- attr(solution, "search_trace")
  if (!.is_search_trace(trace)) {
    stop(
      "`solution` carries no search trace. Call `solve_maze()` with `trace = TRUE`.",
      call. = FALSE
    )
  }

  algorithm_label <- .algorithm_label(trace$algorithm)

  frame_dir <- tempfile("maze-search-frames-")
  dir.create(frame_dir)
  on.exit(unlink(frame_dir, recursive = TRUE, force = TRUE), add = TRUE)

  total_steps <- length(trace$steps)
  frame_steps <- unique(c(seq.int(1L, total_steps, by = every), total_steps))
  frame_count <- length(frame_steps) + hold
  frame_files <- file.path(frame_dir, sprintf("frame-%05d.png", seq_len(frame_count)))

  progress_bar <- NULL
  if (isTRUE(progress)) {
    progress_bar <- utils::txtProgressBar(min = 0L, max = frame_count, style = 3L)
    on.exit(close(progress_bar), add = TRUE)
  }

  closed <- matrix(FALSE, nrow = trace$rows, ncol = trace$cols)
  open <- matrix(FALSE, nrow = trace$rows, ncol = trace$cols)
  open[trace$start[1L], trace$start[2L]] <- TRUE

  # Only bidirectional traces carry `side` on their steps; every other trace
  # has "none" throughout, in which case `side_of_cell` stays NULL and
  # rendering falls back to the original explored/frontier-only shading, so
  # the single-directional case is pixel-for-pixel unchanged.
  has_sides <- any(vapply(trace$steps, function(step) .step_side(step) != "none", logical(1)))
  side_of_cell <- if (has_sides) {
    matrix(NA_character_, nrow = trace$rows, ncol = trace$cols)
  } else {
    NULL
  }

  frame_index <- 0L
  for (step in seq_len(total_steps)) {
    state <- trace$steps[[step]]
    current <- state$current
    open[current[1L], current[2L]] <- FALSE
    closed[current[1L], current[2L]] <- TRUE
    if (nrow(state$opened) > 0L) {
      open[state$opened] <- TRUE
    }
    if (has_sides) {
      step_side <- .step_side(state)
      if (step_side %in% c("forward", "backward")) {
        side_of_cell[current[1L], current[2L]] <- step_side
        if (nrow(state$opened) > 0L) {
          side_of_cell[state$opened] <- step_side
        }
      }
    }

    if (!(step %in% frame_steps)) {
      next
    }
    frame_index <- frame_index + 1L

    .render_search_frame(
      file = frame_files[frame_index],
      maze = maze,
      closed = closed,
      open = open,
      current = current,
      solution = NULL,
      width = width,
      height = height,
      wall_width = wall_width,
      explored_color = explored_color,
      frontier_color = frontier_color,
      current_color = current_color,
      forward_color = forward_color,
      backward_color = backward_color,
      solution_color = solution_color,
      side_of_cell = side_of_cell,
      main = sprintf(
        "%s step %d / %d  |  expanded %d  |  frontier %d",
        algorithm_label, step, total_steps, sum(closed), sum(open)
      )
    )

    if (!is.null(progress_bar)) {
      utils::setTxtProgressBar(progress_bar, frame_index)
    }
  }

  for (i in seq_len(hold)) {
    frame_index <- frame_index + 1L
    .render_search_frame(
      file = frame_files[frame_index],
      maze = maze,
      closed = closed,
      open = open,
      current = NULL,
      solution = solution,
      width = width,
      height = height,
      wall_width = wall_width,
      explored_color = explored_color,
      frontier_color = frontier_color,
      current_color = current_color,
      forward_color = forward_color,
      backward_color = backward_color,
      solution_color = solution_color,
      side_of_cell = side_of_cell,
      main = sprintf(
        "%s solved  |  path %d cells  |  expanded %d of %d",
        algorithm_label, nrow(solution), sum(closed), trace$rows * trace$cols
      )
    )

    if (!is.null(progress_bar)) {
      utils::setTxtProgressBar(progress_bar, frame_index)
    }
  }

  frames <- magick::image_read(frame_files)
  animation <- magick::image_animate(frames, fps = fps)
  magick::image_write(animation, path = output_file, format = "gif")
  output_file <- normalizePath(output_file, winslash = "/", mustWork = TRUE)
  message(sprintf("Animation saved to '%s'.", output_file))
  invisible(output_file)
}

.render_search_frame <- function(file, maze, closed, open, current, solution,
                                 width, height, wall_width, explored_color,
                                 frontier_color, current_color, forward_color,
                                 backward_color, solution_color, side_of_cell,
                                 main) {
  cell_fill <- .search_cell_fill(
    closed, open, current, explored_color, frontier_color, current_color,
    side_of_cell, forward_color, backward_color
  )
  grDevices::png(file, width = width, height = height, bg = "white")
  tryCatch(
    plot_maze(
      maze,
      solution = solution,
      solution_color = solution_color,
      wall_width = wall_width,
      cell_fill = cell_fill,
      main = main
    ),
    finally = grDevices::dev.off()
  )
  invisible(file)
}

.search_cell_fill <- function(closed, open, current, explored_color,
                              frontier_color, current_color,
                              side_of_cell = NULL, forward_color = NULL,
                              backward_color = NULL) {
  if (!is.null(current)) {
    closed[current[1L], current[2L]] <- FALSE
  }
  if (is.null(side_of_cell)) {
    # No trace carries `side` information (every algorithm except the three
    # bidirectional ones) -- shade purely by explored/frontier, exactly as
    # before `side` existed.
    pieces <- list(
      .cells_to_fill(closed, explored_color),
      .cells_to_fill(open, frontier_color)
    )
  } else {
    touched <- closed | open
    forward_mask <- touched & !is.na(side_of_cell) & side_of_cell == "forward"
    backward_mask <- touched & !is.na(side_of_cell) & side_of_cell == "backward"
    other_mask <- touched & !(forward_mask | backward_mask)
    pieces <- list(
      .cells_to_fill(forward_mask, forward_color),
      .cells_to_fill(backward_mask, backward_color),
      .cells_to_fill(other_mask, explored_color)
    )
  }
  if (!is.null(current)) {
    pieces[[length(pieces) + 1L]] <- data.frame(
      row = current[1L],
      column = current[2L],
      color = current_color,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, pieces)
}

.cells_to_fill <- function(flags, color) {
  cells <- which(flags, arr.ind = TRUE)
  data.frame(
    row = as.integer(cells[, 1L]),
    column = as.integer(cells[, 2L]),
    color = rep(color, nrow(cells)),
    stringsAsFactors = FALSE
  )
}

.algorithm_label <- function(algorithm) {
  if (is.null(algorithm)) {
    return("Search")
  }
  labels <- c(
    astar = "A*", dijkstra = "Dijkstra", bfs = "BFS",
    bidirectional_dijkstra = "Bidirectional Dijkstra",
    bidirectional_astar = "Bidirectional A* (NBA*)"
  )
  label <- unname(labels[algorithm])
  if (is.na(label)) algorithm else label
}
