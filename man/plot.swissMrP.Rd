\name{plot}
\alias{plot.swissMrP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots \code{swissMrP} Objects
%%  ~~function to do ... ~~
}
\description{This function plots the results of \code{\link{swissMrP}}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{plot}{swissMrP}(x, conf.int = 0.95, col, design.dot = TRUE, lab, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object returned from \code{swissMrP()}. 
%%     ~~Describe \code{x} here~~
}
  \item{conf.int}{Confidence interval that should be displayed. Only meaningful if x is a matrix with dimensions 26*\code{Number.sim}.
%%     ~~Describe \code{conf.int} here~~
}
  \item{col}{Color for plot.
%%     ~~Describe \code{col} here~~
}

  \item{design.dot}{Logical. If \code{design.dot=TRUE} the plot will display dots. If \code{design.dot=FALSE} the plot will display the cantonal abbreviations (ZH, BE, LU, ...). 
%%     ~~Describe \code{design.dot} here~~
}
  \item{lab}{Label for x-axis. If there is no user specified label the default is \code{Degree of}  \code{Estimated Support}.
%%     ~~Describe \code{design.dot} here~~
}
  \item{\dots}{Further graphical arguments can be used, e.g. \code{main} to specify a title.
%%     ~~Describe \code{\dots} here~~
}
}
\details{The design, when \code{design.dot=FALSE}, was added for Fabio Wasserfallen.
%%  ~~ If necessary, more details than the description above ~~
}
\author{Lucas Leemann, \email{lleemann@gmail.com}
%%  ~~who you are~~
}
\seealso{
	\code{\link{swissMrP}}, \code{\link{map.MrP}}
}

\examples{
\donttest{
# fake data example
vec <- pnorm(rnorm(26))
class(vec) <- "swissMrP"
plot(vec, design.dot=FALSE, main="Simulated Data", 
             lab="Heterogeneous Preferences", col="green")

plot(vec, design.dot=TRUE, main="Simulated Data", 
             lab="Heterogeneous Preferences", col="blue")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Ploting}
\keyword{MrP}% __ONLY ONE__ keyword per line
