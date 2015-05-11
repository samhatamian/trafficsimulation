\name{runBMLGrid}
\alias{runBMLGrid}

\title{
runBMLGrid
}
\description{
Take in createBMLGrid() output and simulate traffic flow for a specified period. 
}
\usage{
runBMLGrid(positionG, totaltime = 5)
}

\arguments{
  \item{positionG}{
createBML() output
}
  \item{totaltime}{
number of iterations
}
\details{

}
\value{
returns a BMLGrid class. Basically a list of positions and the dimensions
}


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (positionG, totaltime = 5) 
{
    r <- positionG[[2]]
    c <- positionG[[3]]
    positions <- positionG[[1]]
    posfinal <- list(positions)
    for (time in 1:totaltime) {
        redCord <- PosToCord(positions[[1]], r)
        blueCord <- PosToCord(positions[[2]], r)
        newcords <- MoveCord(redCord, blueCord, time, r, c)
        positions <- cordToPos(newcords[[1]], newcords[[2]], 
            r)
        posfinal <- append(posfinal, list(positions))
    }
    yy <- list(posfinal, r, c)
    class(yy) <- c("BMLgrid", class(yy))
    yy
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
