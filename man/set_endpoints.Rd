\name{set_endpoints}
\alias{set_endpoints}
\title{Choose new start/end cells for a maze}
\usage{
set_endpoints(maze, strategy = c("longest", "corners", "random", "farthest"),
  start = NULL, end = NULL)
}
\arguments{
\item{maze}{A \code{maze} object, as returned by \code{generate_maze()}.}
\item{strategy}{One of \code{"longest"}, \code{"farthest"},
\code{"corners"}, or \code{"random"}:
\itemize{
  \item \code{"longest"} (the default): finds the true longest shortest
    path in the maze (its diameter) via double breadth-first search --
    BFS from an arbitrary cell locates a farthest cell \code{A}, then BFS
    from \code{A} locates a farthest cell \code{B} from it;
    \code{start = A}, \code{end = B}.
  \item \code{"farthest"}: keeps the maze's existing \code{start} and
    sets \code{end} to the farthest cell from it. This is the same rule
    \code{generate_maze()} uses by default for non-\code{"dfs"}
    algorithms, exposed here so it can be reapplied after the maze has
    changed (e.g. after \code{braid()} or \code{sparsify()}).
  \item \code{"corners"}: \code{start = c(1, 1)},
    \code{end = c(rows, cols)}.
  \item \code{"random"}: two distinct, uniformly chosen cells.
}}
\item{start, end}{Optional \code{c(row, column)} positions. When supplied,
they override \code{strategy} for that endpoint (validated to lie inside
the maze).}
}
\value{
A modified \code{maze} object of class \code{maze}, with \code{start}
and/or \code{end} replaced. All other fields, including \code{passages},
are unchanged.
}
\description{
Re-labels the \code{start} and \code{end} cells recorded on a \code{maze}
object, without touching its passages. Several strategies are available
for picking cells automatically, and an explicit \code{start} and/or
\code{end} overrides the chosen strategy for that endpoint.
}
\examples{
maze <- generate_maze(10, 10, seed = 42)
longest <- set_endpoints(maze, "longest")
corners <- set_endpoints(maze, "corners")
fixed <- set_endpoints(maze, start = c(1, 1), end = c(2, 2))
}
\seealso{
\code{\link{generate_maze}}, \code{\link{solve_maze}}
}
