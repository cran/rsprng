\name{free.sprng}
\alias{free.sprng}
\title{Free memory used by SPRNG}
\description{
  \code{free.sprng} frees the memory allocated to save the PRNG states by
  SPRNG and calls \code{\link{RNGkind}} to restore R's serial RNG.
}
\usage{
free.sprng (kind.old = c("Marsaglia-Multicarry",
                         "Kinderman-Ramage"))
}
\arguments{
  \item{kind.old}{a length 2 character vector, the old rng kinds
  (possiblly returned by \code{\link{init.sprng}})}
}
\value{
  Return current PRNG state and \code{NULL} if there were no active PRNG
  stream.
}
\details{
  SPRNG allocates some memory to save the state of the underlying RNG
  and the memory must be explicitly freed.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{init.sprng}}, \code{\link{pack.sprng}}
}
\examples{
oldrng <- init.sprng (1, 0, seed = 231, kind = "PMLCG")
runif (10)
free.sprng (oldrng)
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu}
}
\keyword{interface}
\keyword{distribution}
