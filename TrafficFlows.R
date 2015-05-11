

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


imagematrix <- function( redposition, blueposition){
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
    blueINred <- apply(bluecord, 1, function(x) x[1] == redcord[,1] & x[2] ==redcord[,2])
    whichInred <- apply(blueINred, 2, any)
    if(any(whichInred)){
      replace <- bluecordORIG[whichInred]
      bluecord[whichInred] <- replace 
    }
    return(list(redcord, bluecord))
  }
  
  if (time %% 2 == 0){
    redcordORIG <- redcord 
    redcord1 <- redcord[, "Y"] + 1
    logical1 <- redcord1 %in% (column+1)
    redcord1[logical1] <- 1 
    redcord[,"Y"] <- redcord1
    redINblue <- apply(redcord, 1, function(x) x[1] == bluecord[,1] & x[2] == bluecord[,2])
    whichInblue <- apply(redINblue, 2, any)
    if(any(whichInblue)){
      replace1 <- redcordORIG[whichInblue]
      redcord[whichInblue] <- replace1 
    }
    return(list(redcord, bluecord))
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

  imagematrix(redPos,bluePos)
  
  #set our positions
  positions<- list(redPos,bluePos)
  ##########ABOVE IS OUR STARTING POSITION, NOW WE ITERATE 
  
  #set time to be 1
  time=1
  redCord <- PosToCord(positions[[1]],r)
  blueCord <- PosToCord(positions[[2]],r)
  newcords <- MoveCord(redCord,blueCord, time, r, c)
  positions <- cordToPos(newcords[[1]], newcords[[2]],r)
  imagematrix(positions[[1]],positions[[2]])
  

  return(list(Pos, redPos, bluePos))
}



