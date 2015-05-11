

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
  return(list(newRedpos,newBluepos))
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
