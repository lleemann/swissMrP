\name{zip1}
\alias{zip1}
\docType{data}
\title{Shape Files for Swiss Cantons
%%   ~~ data name/kind ... ~~
}
\description{The shape files are from the Swiss government (Swiss Federal Statistics Office, 2013).
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(zip1)}
\format{
	@ students: shape files are slightly more involved as they are \code{SpatialPolygonsDataFrame} objects. You should most likely not have to work with the files themselves.
}
\details{If you are looking at this help file then chance is that something went wrong :-(
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{Swiss Federal Statistics Office, 2013.
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\examples{
# A map of the 26 Swiss cantons:
plot(zip1)
}
\keyword{datasets}
