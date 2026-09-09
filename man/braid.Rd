\name{braid}
\alias{braid}
\title{Braid a maze by removing dead ends}
\usage{
braid(maze, p = 1)
}
\arguments{
\item{maze}{A \code{maze} object, as returned by \code{generate_maze()}.}
\item{p}{Probability, in \code{[0, 1]}, that any given dead end has a wall
knocked out. At the default of \code{1}, every dead end that has an
available wall to open is braided away. At \code{0}, \code{braid()} is a
no-op.}
}
\value{
A modified \code{maze} object of class \code{maze}, with \code{passages},
\code{carve_order}, and \code{walls_removed} updated to reflect the newly
opened walls. All other fields (\code{rows}, \code{cols}, \code{start},
\code{end}, \code{algorithm}, etc.) are unchanged.
}
\description{
Scans a \code{maze} object for dead ends -- cells of degree 1 in the
passage graph, i.e. cells reachable through exactly one open wall -- and,
with probability \code{p}, knocks out one more wall from each, creating a
loop. When a dead end has a neighbor that is itself a dead end, that wall
is preferred, since carving it removes two dead ends for the price of one
wall; otherwise a random closed wall out of the cell is chosen instead.
}
\details{
This is deliberately different from -- and complements -- the
\code{open_walls} argument of \code{generate_maze()}. \code{open_walls}
blindly removes any internal wall of the maze at random, with no regard
for where dead ends are. \code{braid()} is targeted: it only ever opens
walls that eliminate (or shorten) dead ends, which is the classic
"braiding" operation used to turn a perfect maze into a braided one with
fewer, shorter dead ends and more loops. You can use both: generate with
\code{open_walls} for random loops and then call \code{braid()} to
specifically clear out remaining dead ends, or vice versa.

The newly opened walls are appended to \code{carve_order} (so animations
and replays of maze construction remain sensible) and counted in
\code{walls_removed}, on top of any walls already removed by
\code{open_walls} at generation time.
}
\examples{
maze <- generate_maze(10, 10, seed = 42)
braided <- braid(maze)
braided$walls_removed
plot_maze(braided)
}
\seealso{
\code{\link{generate_maze}} for the \code{open_walls} argument, which
removes walls at random rather than targeting dead ends;
\code{\link{sparsify}} for the opposite operation of carving \emph{more}
dead ends out of a maze.
}
