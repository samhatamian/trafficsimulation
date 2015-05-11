\name{moved}
\alias{moved}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
moved function
}
\description{
For a given BMLgrid, you can find the number of cars that moved during a certain time period. 
}
\usage{
moved(out, period)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{out}{
Output after using runBMLGrid function
}
  \item{period}{
which time period do you want to see where the car moved? 
}
}
\details{
}
\value{
returns and integer value describing the number of cars moved
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Sam Hatamian 
}


\seealso{
runBMLGrid()
}
\examples{

moved(out, 3)

## The function is currently defined as
function (out, period) 
{
    positions <- out[[1]]
    redmoved <- positions[[period + 1]][[1]] - positions[[period]][[1]]
    bluemoved <- positions[[period + 1]][[2]] - positions[[period]][[2]]
    allmove <- append(redmoved, bluemoved)
    carsmoved <- length(allmove[allmove != 0])
    return(carsmoved)
  }
}

