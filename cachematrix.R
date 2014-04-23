
##Programming Assignment 2  -- Peer Assessments

## The purpose of these R functions are to cache the time consuming process
## of matrix inversion. This prevents re-computation each time this function is called.

## The first function makeCacheMAtrix creates a matrix, containing a list of functions
##including setting the matrix values (set), getting these values (get) if in cache,
##computing the inverse (setSolve) using the solve function and getting the inverse 
##(getSolve) if in cache. setSolve and getSolve are used by the the second function, 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## This purpose of this R function, cacheSolve, is to either call the matrix inversion
## function, or to retrieve it in cache if the matrix inversion has already been computed.
## This function calls back to functions created in makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}