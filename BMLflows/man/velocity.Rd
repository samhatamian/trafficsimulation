\name{velocity}
\alias{velocity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Velocity 
}
\description{
Gives you a velocity of number of cars moved over total number of cars that could have moved
}
\usage{
velocity(out, period, redcar, bluecar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{out}{
  BML Grid Output 
}
  \item{period}{
  Time period where velocity is counted. 
}
  \item{redcar}{
  Number of Red Cars
  \item{bluecar}{
  Number of Blue Cars
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns an integer value known as velocity. 
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sam Hatamian 
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


## The function is currently defined as
function (out, period, redcar, bluecar) 
{
    positions <- out[[1]]
    redmoved <- positions[[period + 1]][[1]] - positions[[period]][[1]]
    bluemoved <- positions[[period + 1]][[2]] - positions[[period]][[2]]
    allmove <- append(redmoved, bluemoved)
    carsmoved <- length(allmove[allmove != 0])
    if (period\%\%2 != 0) {
        velocity <- carsmoved/bluecar
    }
    if (period\%\%2 == 0) {
        velocity <- carsmoved/redcar
    }
    return(velocity)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
