\name{print}
\alias{print.swissMrP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print Function for \code{swissMrP} Objects
%%  ~~function to do ... ~~
}
\description{Prints.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{print}{swissMrP}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{swissMrP}.
%%     ~~Describe \code{x} here~~
}
  \item{...}{additional arguments to be passed to \code{print} function.
%%     ~~Describe \code{x} here~~
}
}

\author{Lucas Leemann
%%  ~~who you are~~
}
\seealso{
	\code{\link{swissMrP}}
}
\examples{
## fake data
fake.est <- runif(26)
class(fake.est) <- "swissMrP"
print(fake.est)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{print}
