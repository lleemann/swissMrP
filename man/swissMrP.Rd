\name{swissMrP}
\alias{swissMrP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multilevel Regression with Poststratification  for the 26 Swiss Cantons
%%  ~~function to do ... ~~
}
\description{This function provides cantonal estimates of public support based on the MrP procedure. Specifically: this function caries out the third (prediction) and forth step (post-stratification) of MrP.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
swissMrP(response.model, augment.data = NA, augment.row = 0, uncertainty = FALSE,
         Number.sim = 1000, region)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{response.model}{The output object of \code{glmer} or \code{blmer} where one estimates a hierarchical model with random effects for: \code{age}, \code{education}, \code{gender}, \code{canton}, and \code{region}. One can include any level 2 predictors.
%%     ~~Describe \code{response.model} here~~
}
  \item{augment.data}{In case one uses survey data for the generation of \code{response.model} which does not have any respondents from one or more cantons there is a problem. The function does not know the values of the level 2 explanatory variables in the \code{response.model} and would hence produce NA's or wrong results. Users can supply the missing information as \code{augment.data}. If a canton is missing it will be a vector, if more than one is missing it is a matrix where the number of rows equals the number of missing cantons. For each level 2 variable one adds a column (see example) and a preceeding column of 1's.
%%     ~~Describe \code{augment.data} here~~
}
  \item{augment.row}{Integer, between 1 and 26. Indicates the row or rows where the \code{augment.data} will be inserted. If data is missing for BE (second canton) and LU (third canton) one will have to supply a vector \code{c(2,3)}.
%%     ~~Describe \code{augment.row} here~~
}
  \item{uncertainty}{Logical. If \code{uncertainty=TRUE} the function will run \code{Number.sim} simulations to obtain a measure of uncertainty for each cantonal prediction.
%%     ~~Describe \code{uncertainty} here~~
}
  \item{Number.sim}{Integer. Number of simulations to be run for uncertainty measure. Default is set to 1000.
%%     ~~Describe \code{Number.sim} here~~
}
  \item{region}{Vector. The regional grouping is based on the large regions in Switzerland (see Leemann and Wasserfallen, 2013). But for certain questions and models alternative groupings could be beneficial. The user can supply a vector (length 26) with integers from 1 to the maximal number of distict regions whereas the first element indicates the regional group of ZH, the second element implies the regional group of BE, ... etc. (it is the standard order of cantons used by the Federal Statistics Office.)
%%     ~~Describe \code{output} here~~
}
}
\value{
	Creates an object of class \code{swissMrP}. A list where the first element is a vector of length 26 (an estimate for each of the 26 cantons).
}

\references{Gelman, Andrew and Thomas C. Little. 1997. Poststratification Into Many Categories Using Hierarchical Logistic Regression. \emph{Survey Research} 23:127-135.
	
	Jeffrey Lax and Justin Phillips. 2009. How Should We Estimate Public Opinion in The States? \emph{American Journal of Political Science} 53 (1), 107-121.
	
	 Leemann, Lucas and Fabio Wasserfallen. 2016. The Democratic Effect of Direct Democracy. \emph{American Political Science Review} 110(4): 750-762.
}
\author{
Lucas Leemann \email{lleemann@gmail.com}
%%  ~~who you are~~
}

\examples{
\donttest{
library(lme4)
### Fake data
err.ind <- rnorm(1000,sd=4)
woman <- sample(c(0,1),replace=TRUE,size=1000) 
age <- sample(c(1:4),replace=TRUE,size=1000) 
education <- sample(c(1:6),replace=TRUE,size=1000) 
cantonnr <- sample(c(1:26),replace=TRUE,size=1000) 
region <- sample(c(1:7),replace=TRUE,size=1000) 
x <- cbind(rnorm(26),rnorm(26)); err.con <- rnorm(26,sd=4); X <- matrix(NA,1000,2)
for (q in 1:1000){ X[q,] <- c(x[cantonnr[q]],err.con[cantonnr[q]])}
y.fake <- X[,1] +X[,2] + woman+age+education+cantonnr+region + err.ind
y <- rep(0,length(y.fake))
y[y.fake>mean(y.fake)]<-1
model1 <- glmer(y ~ X[,1] +X[,2] + (1|woman)  + (1|education) + (1|age) + (1|cantonnr) 
                + (1|region), family=binomial(probit))
# use the MrP function
mrp1 <- swissMrP(model1)

## Here is an example if two cantons (10,22; FR & VD) are missing
mrp2 <- swissMrP(model1,
			augment.data=matrix(c(c(1,2,-1),c(1,1,-5)),2,3, byrow=TRUE),
			augment.row=c(10,22))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{MrP}
\keyword{poststratification}
\keyword{prediction}
\keyword{Estimating Public Opinion}% __ONLY ONE__ keyword per line
