\name{plot_maze}
\alias{plot_maze}
\alias{plot.maze}
\title{Plot a maze}
\usage{
plot_maze(maze, solution = NULL, show_path = FALSE, show_nodes = FALSE,
  wall_color = "#171717", background_color = "white", wall_width = 3,
  path_color = "#8b5cf6", solution_color = "#f59e0b", path_width = 2,
  mark_endpoints = TRUE)

\method{plot}{maze}(x, ...)
}
\arguments{
\item{maze}{A \code{maze} object returned by \code{generate_maze()}.}
\item{solution}{Optional two-column matrix or data frame returned by
\code{solve_maze()}.}
\item{show_path}{Draw the complete generation traversal.}
\item{show_nodes}{Number the cells in the generation traversal.}
\item{wall_color, background_color}{Colors used for walls and cells.}
\item{wall_width}{Width of wall lines.}
\item{path_color, solution_color}{Colors used for optional path overlays.}
\item{path_width}{Width of path overlays.}
\item{mark_endpoints}{Mark and label the start and end cells.}
\item{x}{A \code{maze} object.}
\item{...}{Additional arguments passed to \code{plot_maze()}.}
}
\value{The maze, invisibly.}
\description{
Draws a maze with base R graphics. The generation traversal or a solved path
can optionally be overlaid on the passages.
}
\examples{
maze <- generate_maze(10, 10, seed = 42)
plot_maze(maze)
plot(maze)
}
