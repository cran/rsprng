\name{spawn.new.sprng}
\alias{spawn.new.sprng}
\title{Spawn several new random number streams.}
\description{
  \code{spawn.new.sprng} generates several new PRNG streams at one pass.
}
\usage{
spawn.new.sprng (nstream, seed = 0, kindprng = "default", para = 0)
}
\arguments{
  \item{nstream}{number of new streams to spawn}
  \item{seed}{an integer of random number seed.  It is not the starting
    state of the sequence; rather, it is an encoding of the starting
    state. The same seed for all the streams.  Distinct streams are
    returned.  Only the 31 least significant bits of seed are used in
    determining the initial starting state of the stream.  Default value
    is 0 where the default seed for each random number generator is
    used}
  \item{kindprng}{a character string of the disired kind of parallel
    random number generator}
  \item{para}{additional parameters for the parallel random number
    generators.  If \code{para} is 0, default parameters for each PRNG
    are used.  When invalid parameter is given, a warning is issued and
    the default paramter is used.}
}
\details{
  Usually one would call \code{\link{init.sprng}} separately for each
  process or use \code{\link{spawn.sprng}} to spawn new streams from
  current stream.  \code{spawn.new.sprng} generates new streams
  unrelated to the current one (might be even of different type). Unlike
  \code{\link{init.sprng}}, \code{\link{RNGkind}} is \emph{not} called.
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
  \code{\link{spawn.sprng}}
}
\examples{
prngs <- spawn.new.sprng (3, kind = "MLFG")
unpack.sprng (prngs[,2])
RNGkind ("user")
runif (10)
free.sprng ()
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu}
}
\keyword{interface}
\keyword{distribution}
