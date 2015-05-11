

#function that takes a position and converts it to two vectors, one with X coordinate and one with Y coordinate)

PosToCord <- function(positionvector, row){
  remainX <- positionvector %% row 
  log <- remainX %in% 0 
  remainX[log] <- row
  
  remainY <- ceiling(positionvector/row)
  XY <- cbind(remainX, remainY)
  colnames(XY) <- c('X',"Y")
  return(XY)
}

#create an image of our current position matrix
#now we have positions and corresponding cordinates for red and blue 
#we implement our matrix below by assigning 1 to red and 2 to blue
#we also alter our matrix because the image function takes in the matrix differently


imagematrix <- function(redposition, blueposition, r, c){
  
  grid <- matrix(0, nrow=r, ncol=c)
  grid[redposition] <- 1
  grid[blueposition] <- 2
  
  AltGrid <- t(grid[r:1,])
  image(AltGrid, col =c("white","red","blue"))
}

MoveCord <- function(redcord, bluecord, time, row, column){
  if (time %% 2 != 0 ){
    bluecordORIG <- bluecord 
    bluecord1 <- bluecord[,"X"] - 1
    logical <- bluecord1 %in% 0
    bluecord1[logical] <- row
    bluecord[,"X"] <- bluecord1
    fullmat <- rbind(bluecordORIG, redcord, bluecord)
    logical1 <- duplicated(fullmat)
    fullrow <- nrow(bluecordORIG) + nrow(redcord)
    shortlogical <- logical1[-(1:fullrow)]
    if (any(shortlogical)){
      update <- bluecord[!shortlogical,]
      bluecordORIG[!shortlogical,] <- update
      bluecord <- bluecordORIG
    }
    
    return(list(redcord, bluecord))
    #blueINred <- apply(bluecord, 1, function(x) x[1] == redcord[,1] & x[2] == redcord[,2])
    #whichInred <- apply(blueINred, 2, any)
    #if(any(whichInred)){
     #replace <- bluecordORIG[whichInred]
      #bluecord[whichInred] <- replace 
    #}
    #making sure there are no repeating, when one eats the other 
    #dup <- as.list(data.frame(t(bluecord[duplicated(bluecord),])))
    #bluelist <- as.list(data.frame(t(bluecord)))
    #logicaldup <- bluelist %in% dup 
    #if(any(logicaldup)){
      #bluecord[logicaldup,] <- bluecordORIG[logicaldup,]
    #}
    
  }
  
  if (time %% 2 == 0){
    redcordORIG <- redcord 
    redcord1 <- redcord[, "Y"] + 1
    logical11 <- redcord1 %in% (column+1)
    redcord1[logical11] <- 1 
    redcord[,"Y"] <- redcord1
    fullmat1 <- rbind(redcordORIG, bluecord, redcord)
    logical2 <- duplicated(fullmat1)
    fullrow1 <- nrow(redcordORIG) + nrow(bluecord)
    shortlogical1 <- logical2[-(1:fullrow1)]
    if (any(shortlogical1)){
      update1 <- redcord[!shortlogical1,]
      redcordORIG[!shortlogical1,] <- update1
      redcord <- redcordORIG
    }
    return(list(redcord, bluecord))
    #redINblue <- apply(redcord, 1, function(x) x[1] == bluecord[,1] & x[2] == bluecord[,2])
    #whichInblue <- apply(redINblue, 2, any)
    #if(any(whichInblue)){
    #  replace1 <- redcordORIG[whichInblue]
    #  redcord[whichInblue] <- replace1 
    #}
    
    #making sure there are no repeats when one eats the other 
    #dup1 <- as.list(data.frame(t(redcord[duplicated(redcord),])))
    #redlist <- as.list(data.frame(t(redcord)))
    #logicaldup1 <- redlist %in% dup1
    #if(any(logicaldup1)){
    #  redcord[logicaldup1,] <- redcordORIG[logicaldup1,]
    #}
  }   
}


########


cordToPos <- function(REDCORD,BLUECORD, row){
  newRedpos <- REDCORD[,"X"] + ((REDCORD[,"Y"] -1)*row)
  newBluepos <- BLUECORD[, "X"] + ((BLUECORD[,"Y"] -1)*row)
  retrurn(list(newRedpos,newBluepos))
}


createBMLGrid<- function(r=3, c=3, ncars = c(red=2,blue=2)){
  
  if (ncars[1]+ncars[2] > r*c){
    stop("Too many cars were selected!")
  }
  
  #creating a vector of positions based on total length of grid and then sample randomly
  #by total number of cars
  totallength <- r*c 
  totalcars <- ncars[1]+ncars[2]
  OrdPos <- c(1:(totallength))
  #randomly sample but do not repeat
  Pos <- sample(OrdPos, totalcars, replace=FALSE)
  #assign random positions to car color
  redPos <- Pos[1:ncars[1]]
  bluePos <- Pos[(ncars[1]+1):totalcars] 
  
  #get the image of our position 
  
  imagematrix(redPos,bluePos, r, c)
  
  #set our positions
  positions<- list(redPos,bluePos)
  return(list(positions = positions, r = r, c = c))
}


plot.BMLgrid <- function(x, ...){
  finalposition <- x[[1]]
  r <- x[[2]]
  c <- x[[3]]
  sapply(finalposition, function(x) imagematrix(x[[1]],x[[2]], r, c))
}


runBMLGrid <- function(positionG, totaltime=5) {
  r <- positionG[[2]]
  c <- positionG[[3]]
  positions <- positionG[[1]]
  posfinal <- list(positions)
  
  for (time in 1:totaltime){
    redCord <- PosToCord(positions[[1]],r)
    blueCord <- PosToCord(positions[[2]],r)
    newcords <- MoveCord(redCord,blueCord, time, r, c)
    positions <- cordToPos(newcords[[1]], newcords[[2]],r)
    posfinal <- append(posfinal, list(positions))
    #imagematrix(positions[[1]],positions[[2]], r, c)
  }
  yy <- list(posfinal, r, c)
 # class(yy) <- 'imatrix'
  class(yy) <- c("BMLgrid", class(yy))
  yy
}

moved <- function(out, period){
  positions <- out[[1]]
  redmoved <- positions[[period +1]][[1]] - positions[[period]][[1]]
  bluemoved <- positions[[period +1]][[2]] - positions[[period]][[2]]
  allmove <- append(redmoved,bluemoved)
  carsmoved <- length(allmove[allmove!=0])
  return(carsmoved)
}

blocked <- function(output, period){
  positions <- output[[1]]
  if (period %% 2 != 0){
    shift <- positions[[period+1]][[2]] - positions[[period]][[2]]
    shift1 <- shift[shift==0] 
    block <- length(shift1)
  }
  if (period %% 2 == 0){
    shift <- positions[[period+1]][[1]] - positions[[period]][[1]]
    shift1 <- shift[shift==0] 
    block <- length(shift1)
  }
  return(block)
}


velocity <- function(out, period, redcar, bluecar){
  positions <- out[[1]]
  redmoved <- positions[[period +1]][[1]] - positions[[period]][[1]]
  bluemoved <- positions[[period +1]][[2]] - positions[[period]][[2]]
  allmove <- append(redmoved,bluemoved)
  carsmoved <- length(allmove[allmove!=0])
  if (period %% 2!= 0){
    velocity <- carsmoved/bluecar
  }
  if (period %% 2==0){
    velocity <- carsmoved/redcar
  }
  return(velocity)
}


summary.BMLgrid <- function(Gout,...){
  lastpos <- Gout[[1]]
  lastnum <- length(lastpos)
  finalgrid <- lastpos[[lastnum]]
  redcar <- length(finalgrid[[1]])
  bluecar <- length(finalgrid[[2]])
  velocity <- velocity(Gout, lastnum-1, redcar, bluecar)
  gridsize <- c(Gout[[2]], Gout[[3]])
  text1 <- cat(" The number of red cars is/are", redcar, ".\n")
  text2 <- cat(" The number of blue cars is/are", bluecar, ".\n")
  text3 <- cat(" The grid size is", gridsize, ".\n")
  text4 <- cat(" The velocity in the last position is", velocity, ".\n")
}


#here is where I optimize the function
#make sure to print this output
Rprof()
###run function 
Rprof(NULL)
summaryRprof()
  

#I adjust these parameters multiple times when running my code but below is just a test
#Here is an example where I actually end up making multiple GIFS by changing outputs


# after saving all functions above this is where you can run the function. below is an example of a grid that 
# is about 1000 spaces in length with an equal number of red and blue cars
#feel free to change the parameters yourself below
library(animation)
positiongrid <- createBMLGrid(31, 31, ncars=c(400,400))
g.out = runBMLGrid(positiongrid, 100)
#plot(g.out) #this will plot every grid in R and will take a long time. Just save it as a GIF instead
summary(g.out)
#below will run the gif if you have the installed software. If not I would advise
#using the plot function but changing your runBMLGrid paramter to something small like 10 above. 
saveGIF(plot(g.out), movie.name = 'mygif.gif', interval=0.2)
#moved blocked and velocity tells you at a specific time, how many cars moved, were blocked etc. 
moved(g.out,5)
blocked(g.out,5)
velocity(g.out,200,1750,1750)

#blocks by proportion I just went through this function and changed the parameters to get the averages
positiongrid <- createBMLGrid(31,31, ncars=c(450,50))
g.out = runBMLGrid(positiongrid, 100)

block <- rep(0,100)
for (i in 1:100){
  block[i] <- blocked(g.out,i)
}

avg1 <- mean(block) # at .10 #117
avg2 <- mean(block) # at .20 #144
avg3 <- mean(block) # at .50 #166
avg4 <- mean(block) # at .80 #139
avg5 <- mean(block) # at .90 #104

prop <- c(avg1,avg2,avg3,avg4,avg5)
xcord <- c(.10, .20, .50, .80, .90)

plot(xcord, prop, type="b", xlab="prop of reds", ylab="average number blocked", main="AVG Block based on color proportions over 100 steps")


#iterating velocity over time for specific cases 
par(mfrow = c(2, 1))
positiongrid <- createBMLGrid(5, 5, ncars=c(10,10))
g.out = runBMLGrid(positiongrid, )

veloc <- rep(0,100)

for (i in 1:100){
  veloc[i] <-velocity(g.out,i, 300, 300)
}

plot(veloc,type="b", xlab="time", main="Velocity over time for 1000 grid with 400 R and 200 B")

positiongrid <- createBMLGrid(100, 100, ncars=c(4000,2000))
g.out = runBMLGrid(positiongrid, 100)
veloc <- rep(0,100)

for (i in 1:100){
  veloc[i] <-velocity(g.out,i, 3000, 3000)
}


plot(veloc, type="b", xlab="time", main="Velocity over time for 10000 grid with 4000 R and 2000 B")



#timing of function 

Rprof()
runBMLGrid(positiongrid, 100)
Rprof(NULL)
summaryRprof()
system.time(runBMLGrid(positiongrid, 500))

#100 , 35 35, 0.68 seconds 
#1000 5.471
#5000 32.358 
#100000 35 35, 63 seconds 

#I plug in various grid values into my function from above and return these vectors then I plot

x <- c(100, 1000, 5000, 10000)
y <- c(0.68, 5.471, 32.358, 63)

plot(x,y, type='b', xlab="gridsize", ylab="time", main="Time it takes to process function")

library(BMLflows)



