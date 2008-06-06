\name{spawn.sprng}
\alias{spawn.sprng}
\title{Spawn new random number streams based on the current one.}
\description{
  Some times when a process spawns children processes, it is desirable
  to spawn new streams from an old one and pass them on to children.
  \code{spawn.sprng} creates new random number streams based on the
  current one, and returns them as columns of an integer matrix.
  These new streams can be then transfered to children processes.
}
\usage{
spawn.sprng (nspawn)
}
\arguments{
  \item{nspawn}{number of new streams to spawn}
}
\value{
  Return an integer matrix with \code{nstream} columns, each column
  corresponds to one stream.  The number of rows will depend on the type
  of the PRNG.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{pack.sprng}}, \code{\link{unpack.sprng}},
  \code{\link{spawn.new.sprng}}
}
\examples{
init.sprng (1, 0, kind = "CMRG")
child.rngs <- spawn.sprng (3)
unpack.sprng (child.rngs[,1])
runif (10)
unpack.sprng (child.rngs[,2])
runif (10)
free.sprng ()
}
\author{
  Na (Michael) Li \email{nali@umn.edu}
}
\keyword{interface}
\keyword{distribution}
