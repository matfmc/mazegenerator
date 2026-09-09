#' Animate maze generation
#'
#' Renders successive generation states and writes them to an animated GIF.
#' The optional `magick` package is required only when this function is called.
#'
#' @param maze A `maze` object returned by [generate_maze()].
#' @param output_file Destination path for the GIF.
#' @param fps Frames per second.
#' @param every Render every nth generation step. Increase this for large mazes.
#' @param width,height Frame dimensions in pixels.
#' @param wall_width Width of wall lines.
#' @param progress Show a progress bar in interactive sessions.
#'
#' @return The normalized output path, invisibly.
#' @export
#'
#' @examples
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   maze <- generate_maze(5, 5, seed = 42)
#'   animate_maze(maze, tempfile(fileext = ".gif"), every = 2)
#' }
animate_maze <- function(maze, output_file = "maze-animation.gif", fps = 10,
                         every = 1L, width = 800L, height = 800L,
                         wall_width = 3, progress = interactive()) {
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

  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    stop("The directory containing `output_file` does not exist.", call. = FALSE)
  }

  frame_dir <- tempfile("maze-frames-")
  dir.create(frame_dir)
  on.exit(unlink(frame_dir, recursive = TRUE, force = TRUE), add = TRUE)

  total_steps <- length(maze$carve_order)
  frame_steps <- if (total_steps == 0L) {
    0L
  } else {
    unique(c(seq.int(1L, total_steps, by = every), total_steps))
  }
  frame_files <- file.path(frame_dir, sprintf("frame-%05d.png", seq_along(frame_steps)))

  progress_bar <- NULL
  if (isTRUE(progress)) {
    progress_bar <- utils::txtProgressBar(min = 0L, max = length(frame_steps), style = 3L)
    on.exit(close(progress_bar), add = TRUE)
  }

  for (i in seq_along(frame_steps)) {
    step <- frame_steps[i]
    partial <- maze
    partial$traversal <- NULL
    partial$visitation_order <- NULL
    partial$passages <- maze$carve_order[seq_len(step)]
    partial$end <- maze$end

    if (step == total_steps) {
      partial$passages <- maze$passages
    }

    grDevices::png(frame_files[i], width = width, height = height, bg = "white")
    tryCatch(
      plot_maze(partial, wall_width = wall_width),
      finally = grDevices::dev.off()
    )

    if (!is.null(progress_bar)) {
      utils::setTxtProgressBar(progress_bar, i)
    }
  }

  frames <- magick::image_read(frame_files)
  animation <- magick::image_animate(frames, fps = fps)
  magick::image_write(animation, path = output_file, format = "gif")
  output_file <- normalizePath(output_file, winslash = "/", mustWork = TRUE)
  message(sprintf("Animation saved to '%s'.", output_file))
  invisible(output_file)
}
