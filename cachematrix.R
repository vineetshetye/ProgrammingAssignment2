## Programming Assignment 2: Saving the inverse of a matrix in cache
## 24th April 2014

## makeCacheMatrix function creates a list of functions which intialise
## and retrieve the value matrix and its inverse 

makeCacheMatrix<-function(x = matrix()) {
  Invs <- NULL
  set <- function(y) {
    x <<- y
    Invs <<- NULL
  }
  get <- function() x
  setinv <- function(inv) Invs <<- inv
  getinv <- function() Invs
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cachesolve functions first checks if the Inverse value is already available
## in cache, else it retrieves the matrix data by calling the above function, 
## calculates the inverse matrix and saves it in cache and returns the inverse 
## of the matrix

cachesolve<-function(x, ...) {
  Invs <- x$getinv()
  if(!is.null(Invs)) {
    message("getting cached data")
    return(Invs)
  }
  data <- x$get()
  Invs <- solve(data, ...)
  x$setinv(Invs)
  Invs
}
