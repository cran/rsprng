\name{type.sprng}
\alias{type.sprng}
\title{Return the type of the active SPRNG stream}
\description{
  \code{type.sprng} returns the type of current active SPRNG stream.
}
\usage{
type.sprng ()
}
\value{
  Return the a string, the type of active SPRNG.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{init.sprng}},
}
\examples{
init.sprng (1, 0, kind = "MLFG", para = 6)
type.sprng ()
free.sprng ()
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{interface}
\keyword{distribution}
