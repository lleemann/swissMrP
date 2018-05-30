\name{map.MrP}
\alias{map.MrP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Map for MrP Estimates
%%  ~~function to do ... ~~
}
\description{This is a simple wrapper that reads in shape files and creates an ordinary map. The displayed map will show the intensity of support for each canton. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
map.MrP(x, colors.m, threshold, main, labels = TRUE, legend.text1, legend.text2, ...)
}
\arguments{
  \item{x}{The output object from \command{\link{swissMrP}} whether it is a vector or a  matrix (the latter if simulations were ran in \code{swissMrP}). 
%%     ~~Describe \code{x} here~~
}
  \item{colors.m}{Vector of colors to be used in increasing order. If the user does not supply any colors, the plot will use a default option (from red to green).
}
  \item{threshold}{Vector. By default the map breaks support in ten equal-sized intervals. If one supplies colors, the support will be broken into \code{length(colors.m)} equal intervals. If the user wishes to change the intervals this is done by supplying a vector with interval boundaries starting with 0. For \emph{n} intervals one needs \emph{n+1} bondaries.
}
  \item{main}{User may specify a title for the plot.
}
  \item{labels}{Logical. User may specify whether labels should be written automatically or if they should be omitted.
}
  \item{legend.text1}{User may specify a legend, e.g. \emph{Support Smoking Ban} to declare what the colors indicate.
%%     ~~Describe \code{col.strength} here~~
}
  \item{legend.text2}{Like \code{legend.text1}, offers a second line.
%%     ~~Describe \code{col.strength} here~~
}
  \item{\dots}{additional arguments to be passed to the low level plotting functions.
%%     ~~Describe \code{\dots} here~~
}
}


\author{Lucas Leemann
%%  ~~who you are~~
}

\note{The shape files for this command are from the Swiss government (Swiss Federal Statistics Office, 2013.). While this function only needs the cantonal boundaries one can also download the shape files for municipality boundaries at the above mentioned URL. The actual mapping is done with the plot command based on elements from the \code{maptools} package.
%%  ~~further notes~~
}
\seealso{
	\code{\link{zip1}}
	\code{\link{swissMrP}}
}
\examples{
# Vanilla example
fake.pref <- runif(26)
class(fake.pref) <- "swissMrP"
\donttest{
map.MrP(fake.pref, main="This Map Shows Random Data", 
        legend.text1="Support for Anything")
        }
## changing intervals
\donttest{
map.MrP(fake.pref, 
        threshold=c(0,0.3,0.45,0.48,0.49,0.5,0.51,0.52,0.55,0.7,1))
        }
## no labels
\donttest{
map.MrP(fake.pref, main="This Map Shows Random Data", 
        legend.text1="Support for Anything", labels=FALSE)
        }
## specify different colors and less groups
\donttest{
map.MrP(fake.pref, main="This Map Shows Random Data", 
        legend.text1="Support for Anything", 
        colors.m=c("skyblue","skyblue1","skyblue2","skyblue3",
        "slateblue","slateblue1","slateblue2"))
        }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Map}
\keyword{Plot}% __ONLY ONE__ keyword per line
