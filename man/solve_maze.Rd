\name{solve_maze}
\alias{solve_maze}
\title{Solve a maze or walkable matrix with A-star search}
\usage{
solve_maze(x, start = NULL, end = NULL, walkable = 1,
  diagonal = FALSE)
}
\arguments{
\item{x}{A \code{maze} object or a matrix.}
\item{start, end}{Integer vectors \code{c(row, column)}. For a maze, they
default to its recorded start and end. They are required for a matrix.}
\item{walkable}{Values considered walkable when \code{x} is a matrix.}
\item{diagonal}{Allow diagonal movement for a matrix. Maze passages are always
orthogonal.}
}
\value{
A data frame with columns \code{row} and \code{column}, ordered from start to
end.
}
\description{
For a \code{maze} object, the function follows its passages. A numeric or
logical matrix can also be supplied, in which case cells matching
\code{walkable} may be crossed.
}
\examples{
maze <- generate_maze(10, 10, seed = 42)
path <- solve_maze(maze)
plot_maze(maze, solution = path)
}
