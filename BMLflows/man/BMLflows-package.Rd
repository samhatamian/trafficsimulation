\name{BMLflows-package}
\alias{BMLflows-package}
\alias{BMLflows}
\docType{package}
\title{
BMLflows Package! 

}
\description{
BMLflows allows for a user to model traffic simulation by specifiying a certain number of red and blue cars over a time period. The cars move based on rules in which blue cars move up in the 1st, 3rd, 5th etc. time period and red cars move in the 2nd, 4th, 6th time period. BMLflows also modifies the plot function so a user can plot the simulation as well. 
}
\details{
\tabular{ll}{
Package: \tab BMLflows\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-04-29\cr
License: \tab MIT\cr
}
~~ Important functions include velocity, createBMLGrid and runBMLgrid ~~
}
\author{
Sam Hatamian

Maintainer: Sam Hatamian <sdhatamian@ucdavis.edu>

}
\examples{
positiongrid <- createBMLGrid(3, 3, ncars=c(2,4))
g.out = runBMLGrid(positiongrid, 10)
plot(g.out)
summary(g.out)
}
