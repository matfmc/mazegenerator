\name{plot_maze}
\alias{plot_maze}
\alias{plot.maze}
\title{Plot a maze}
\usage{
plot_maze(maze, solution = NULL, show_path = FALSE, show_nodes = FALSE,
  wall_color = "#171717", background_color = "white", wall_width = 3,
  path_color = "#8b5cf6", solution_color = "#f59e0b", path_width = 2,
  cell_fill = NULL, mark_endpoints = TRUE, main = NULL)

\method{plot}{maze}(x, ...)
}
\arguments{
\item{maze}{A \code{maze} object returned by \code{generate_maze()}.}
\item{solution}{Optional two-column matrix or data frame returned by
\code{solve_maze()}.}
\item{show_path}{Draw the complete generation traversal. Only available
for mazes generated with \code{algorithm = "dfs"}.}
\item{show_nodes}{Number the cells in the generation traversal. Only
available for mazes generated with \code{algorithm = "dfs"}.}
\item{wall_color, background_color}{Colors used for walls and cells.}
\item{wall_width}{Width of wall lines.}
\item{path_color, solution_color}{Colors used for optional path overlays.}
\item{path_width}{Width of path overlays.}
\item{cell_fill}{Optional data frame with \code{row}, \code{column} and
\code{color} columns. The named cells are shaded before the walls are drawn,
which is how \code{animate_solution()} paints the explored cells and the
frontier.}
\item{mark_endpoints}{Mark and label the start and end cells.}
\item{main}{Plot title. Defaults to the maze dimensions.}
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

plot_maze(
  maze,
  cell_fill = data.frame(row = 1:3, column = 1L, color = "#bae6fd")
)
}
