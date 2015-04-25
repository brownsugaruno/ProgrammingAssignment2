## My functions create a cachable inverse matrix, in order to conduct the calculations
## in a faster manner, saving on time using R functions to cache the computations.

## This function creates a special "matrix" which is a list containing a function to
## set and get the value of the matrix, and set and get the value of its inverse.

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


## The following function calculates the inverse of the special "matrix" created above.
## It first checks if the inverse has already been calculated, and if yes, it retrieves the answer from cache.
## If it has not been computed yet, it calculates the inverse and sets the value of the inverse in the cache with the 
## setsolve function.

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
}
