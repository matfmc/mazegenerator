\name{animate_maze}
\alias{animate_maze}
\title{Animate maze generation}
\usage{
animate_maze(maze, output_file = "maze-animation.gif", fps = 10,
  every = 1L, width = 800L, height = 800L, wall_width = 3,
  progress = interactive())
}
\arguments{
\item{maze}{A \code{maze} object returned by \code{generate_maze()}.}
\item{output_file}{Destination path for the GIF.}
\item{fps}{Frames per second.}
\item{every}{Render every nth generation step. Increase this for large mazes.}
\item{width, height}{Frame dimensions in pixels.}
\item{wall_width}{Width of wall lines.}
\item{progress}{Show a progress bar in interactive sessions.}
}
\value{The normalized output path, invisibly.}
\description{
Renders successive generation states and writes them to an animated GIF. The
optional \pkg{magick} package is required only when this function is called.
}
\examples{
if (requireNamespace("magick", quietly = TRUE)) {
  maze <- generate_maze(5, 5, seed = 42)
  animate_maze(maze, tempfile(fileext = ".gif"), every = 2)
}
}
