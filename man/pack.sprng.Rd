\name{pack.sprng}
\alias{pack.sprng}
\title{Return the PRNG state.}
\description{
  \code{pack.sprng} returns the state of current active PRNG
  stream as a vector of integers.
}
\usage{
pack.sprng ()
}
\value{
  Return the current PRNG state as an integer vector or \code{NULL} if
  there is no active PRNG stream.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{unpack.sprng}}, \code{\link{spawn.sprng}}
}
\examples{
init.sprng (1, 0, kind = "LCG")
runif (10)
saved <- pack.sprng ()
a1 <- runif (10)
unpack.sprng (saved)
a2 <- runif (10)
all (a1 == a2)
free.sprng ()
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu}
}
\keyword{distribution}
\keyword{interface}
