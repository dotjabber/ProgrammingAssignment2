## Maciej Kowalski
## Coursera // R Programming // Assignment 2

## General idea is exactly the same as in assignment - there is a list 
## created, containing:
## - getter and setter to given matrix
## - getter and setter to inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
  }
  
  get <- function() {
    x
  }
  
  setInv <- function(inv) {
    i <<- inv
  }
  
  getInv <- function() {
    i
  }
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## First of all, we need a matrix, we can create it by calling 
## makeCacheMatrix() function. 
## Such created matrix is then given to this method, if it finds
## i <- x$getInv(), then your inverse matrix is taken from cache
## otherwise, it will be computed.
##
## usage:
## m1 <- matrix(sample(25), ncol = 5, nrow = 5) # make regular matrix
## e1 <- makeCacheMatrix(m1) # make cached matrix
## i1 <- cacheSolve(e1); # if you run this for first time, it will spit no message
## i1 <- cacheSolve(e1); # if you'll do it again, it will give you proper message :)
## et voila :)
cacheSolve <- function(x, ...) {
  i <- x$getInv()
  
  if(!is.null(i)) {
    message("getting cached data - message just to be certain that it is comming from cache :)")
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setInv(i)
  i
}