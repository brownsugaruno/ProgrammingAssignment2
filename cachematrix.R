## My function creates a cachable inverse matrix, in order to conduct less resource
## demanding calculations in a faster manner.

## This function creates the 'skeleton' or structure of the inverse matrix that
## is to be specified by the next function.

makeCacheMatrix <- function(x = matrix()) {
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


## This specifies the cachable inverse matrix function and returns the
## inverse matrix.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
