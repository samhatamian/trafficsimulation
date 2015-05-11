\name{createBMLGrid}
\alias{createBMLGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create BML Grid
}
\description{
Returns initial BML Grid based on grid size, proportion of cars. Positions are random. 
}
\usage{
createBMLGrid(r = 3, c = 3, ncars = c(red = 2, blue = 2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
    Number of Rows
}
  \item{c}{
     Number of Columns
}
  \item{ncars}{
     A vector describing number of red cars and blue cars respectively.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns list of positions for red and blue cars as well as the dimensions of your grid 
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

positiongrid <- createBMLGrid(10, 10, ncars=c(30,30))
g.out = runBMLGrid(positiongrid, 500)
plot(g.out)

function (r = 3, c = 3, ncars = c(red = 2, blue = 2)) 
{
    if (ncars[1] + ncars[2] > r * c) {
        stop("Too many cars were selected!")
    }
    totallength <- r * c
    totalcars <- ncars[1] + ncars[2]
    OrdPos <- c(1:(totallength))
    Pos <- sample(OrdPos, totalcars, replace = FALSE)
    redPos <- Pos[1:ncars[1]]
    bluePos <- Pos[(ncars[1] + 1):totalcars]
    imagematrix(redPos, bluePos, r, c)
    positions <- list(redPos, bluePos)
    return(list(positions = positions, r = r, c = c))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
