\name{generate_maze}
\alias{generate_maze}
\alias{print.maze}
\title{Generate a random maze}
\usage{
generate_maze(rows = 20L, cols = rows, start = c(1L, 1L),
  seed = NULL, verbose = FALSE)

\method{print}{maze}(x, ...)
}
\arguments{
\item{rows}{Number of rows in the maze.}
\item{cols}{Number of columns in the maze.}
\item{start}{Integer vector \code{c(row, column)} giving the starting cell.}
\item{seed}{Optional integer seed for reproducible generation.}
\item{verbose}{Show generation progress in the console.}
\item{x}{A \code{maze} object.}
\item{...}{Additional arguments, currently unused.}
}
\value{
An object of class \code{maze} containing its dimensions, traversal, passages,
and start and end cells. The print method returns the object invisibly.
}
\description{
Uses randomized depth-first search with backtracking to generate a perfect
rectangular maze: every cell is reachable and there is exactly one simple path
between any two cells.
}
\examples{
maze <- generate_maze(10, 10, seed = 42)
maze
}
