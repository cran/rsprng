\name{unpack.sprng}
\alias{unpack.sprng}
\title{Restore the PRNG state.}
\description{
  \code{unpack.sprng} unpacks a saved PRNG state and replace the current
  active PRNG state with the new one.
}
\usage{
unpack.sprng (rngstate)
}
\arguments{
  \item{rngstate}{an integer vector representing PRNG state (should be
    returned by either \code{\link{pack.sprng}},
    \code{\link{spawn.sprng}}, \code{\link{spawn.new.sprng}})}
}
\value{
  Return the states of the old PRNG stream if there is one and return
  \code{NULL} if not.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{pack.sprng}}, \code{\link{spawn.sprng}},
  \code{\link{spawn.new.sprng}}
}
\examples{
init.sprng (1, 0, seed = 1132, kind = "LCG64")
runif (5)
rng.saved <- pack.sprng ()
runif (5)
unpack.sprng (rng.saved)
runif (5)
free.sprng ()
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{distribution}
\keyword{interface}
