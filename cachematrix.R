install.packages("MASS")
library(MASS)

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
a <- rbind(c(1, -1/4), c(-1/4, 1))  
a

ainv <- solve(a)

ainv %*% a


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


## Write a short comment describing this function

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

a <- rbind(c(1, -1/4), c(-1/4, 1))
a1 <- rbind(c(1, -1/4, 4.5,33,55), c(-1/4, 1,2,12,34))
x <- makeCacheMatrix(a)
x1 <- makeCacheMatrix(a1)
p1 <- cacheSolve(x1)
p <- cacheSolve(x)

a1 %*% p1

solve(a)

