\name{census}
\alias{census}
\docType{data}
\title{Census File, Switzerland 2000
%%   ~~ data name/kind ... ~~
}
\description{The file contains the cenus information which was retrieved from the BfS (Federal Statistical Office). It consists of three variables; age (4 groups), gender, and education (6 groups) - see \code{details} for more information.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(census)}
\format{
  The format is:
List of 2 Elements

 $ census48.MAZH2013: Matrix 48*26 (48 ideal types, 6*4*2) and 26 cantons. The 26 columns represent the 26 cantons, the 48 rows represent all possible combinations.
 
 \tabular{rrrr}{
 group \tab \code{educ}  \tab  \code{age} \tab  \code{gender}\cr
\code{row 1}: \tab  1 \tab  1 \tab  1\cr
\code{row 2}: \tab  1 \tab  1 \tab  2\cr
\code{row 3}: \tab  1 \tab  2 \tab  1\cr
\code{row 4}: \tab  1 \tab  2 \tab  2\cr
\code{row 5}: \tab  1 \tab  3 \tab  1\cr
\code{row 6}: \tab  1 \tab  3 \tab  2\cr
\code{row 7}: \tab  1 \tab  4 \tab  1\cr
\code{row 8}: \tab  1 \tab  4 \tab  2\cr
\code{row 9}: \tab  2 \tab  1 \tab  1\cr
.... \tab  \tab  \tab \cr
\code{row 46}: \tab  6 \tab  3 \tab  2\cr
\code{row 47}: \tab  6 \tab  4 \tab  1\cr
\code{row 48}: \tab  6 \tab  4 \tab  2\cr
}

 
 $ INFO             : see \code{census$INFO}
}
\details{Education: (1 - if only mandatory school or NA), (2 - Apprenticeship), (3 - High School Diploma or teachers' school), (4 - Higher education, germ: hoehere Fachausbildung), (5 - higher education 2, germ: hoehere Fachschule), (6 - Federal technical Institutes or University of apllied sciences), (7 - University). Age (1: 0-34, 2: 35-49, 3: 50-64, 4: 64-). Woman (0 man, 1 woman).

%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{BfS. Bundesamt fuer Statistik. 
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{Bundesamt fuer Statistik (BfS). 2000. Eidgenoessische Volkszaehlung 2000; Abschlussbericht zur Volkszaehlung.
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
 # How many young men with no higher education than mandatory schooling live in 
 # the cantons  ZH, BE, and LU?
 data(census)
 census$census48.MAZH2013[1:3,1]		# 1st column is ZH
}
\keyword{datasets}
