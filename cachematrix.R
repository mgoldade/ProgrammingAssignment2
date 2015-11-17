## These functions calculate the inverse of a square matrix (we assume that
## the inverse ALWAYS exists) and then cache it in memory so that it can
## be called back without being recalculated

## Calculate the inverse of matrix x. M determines whether or not the inverse
## has been calculated for x or not. solveMatrix is not actually called by
## the user, the functions setsolve and getsolve may be called by cacheInverse

solveMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheInverse is a function which will either calculate the inverse of a square
## matrix (the inverse must exist) (if m is not null as determined by getsolve above)
## or return the already cached inverse of the matrix sent to the function, displaying
## the message 'getting cached data'

cacheInverse <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
