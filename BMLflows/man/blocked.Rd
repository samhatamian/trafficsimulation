\name{blocked}
\alias{blocked}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Blocked Function 
}
\description{
Input your BML Grid output and a period and the output will tell you how many cars were blocked in the next period}
\usage{
blocked(output, period)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{output}{
BML GRID
}
  \item{period}{
Time as an integer 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Integer value of number of cars blocked
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (output, period) 
{
    positions <- output[[1]]
    if (period\%\%2 != 0) {
        shift <- positions[[period + 1]][[2]] - positions[[period]][[2]]
        shift1 <- shift[shift == 0]
        block <- length(shift1)
    }
    if (period\%\%2 == 0) {
        shift <- positions[[period + 1]][[1]] - positions[[period]][[1]]
        shift1 <- shift[shift == 0]
        block <- length(shift1)
    }
    return(block)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
