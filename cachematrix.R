install.packages("MASS")
library(MASS)
## installing package MASS which includes the inverse function

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## First function, makrCacheMatrix functions takes a matrix input, and returns a list with set, get and setInv and getInv functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## second functions takes ainput the list outputted from previous function, checks for the datastored in the cache, and spits the inverse value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- ginv(data, ...)
  x$setInv(m)
  m
}


# tests
#a <- rbind(c(1, -1/4), c(-1/4, 1))
#a1 <- rbind(c(1, -1/4, 4.5,33,55), c(-1/4, 1,2,12,34))

#x <- makeCacheMatrix(a)
#x1 <- makeCacheMatrix(a1)

#p1 <- cacheSolve(x1)
#p <- cacheSolve(x)

