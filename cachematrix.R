## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function makes a matrix x, sets the initial solution to NULL,
# (both x and sol are stored within the internal environment) and returns a
# list of functions that we can later use in cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
  sol <- NULL
  setmat <- function(y) {
    x <<- y
    sol <<- NULL
  }
  getmat <- function() x
  setsol <- function(mysolution) sol <<- mysolution
  getsol <- function() sol
  list(setmat = setmat, getmat = getmat,
       setsol = setsol,
       getsol = getsol)
}


## Write a short comment describing this function
# Note: the x used in cacheSolve is NOT the same as the x used in 
# makeCacheMatrix. This may seem silly but I was stumped for a while trying
# to figure out the sample code!
# Basically does the same thing as cachemean, but for the matrix analog.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  sol <- x$getsol()
  if (!is.null(sol)) {
    message("Getting cached data!")
    return (sol)
  }
  data <- x$getmat()
  sol <- solve(data, ...)
  x$setsol(sol)
  sol
}
