## The following 2 functions can be used to return the inversion of a matrix;
## if the inversion was already computed, it will be fetched from the cache.

## makeCacheMatrix() gets a matrix, and returns a special list that can be used
## by cachesolve() to return the inversio of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # Create a variable named 's', that will keep the cached reverse matrix
  s <- NULL
  # Now create 4 funcrions that will be used to work with CacheMatrix
  # The set function should be used whenever we want to set the matrix
  set <- function(y) {
    x <<- y
    # set initializes the solution to NULL (since the matrix was changed,
    # the solution is not cached anymore)
    s <<- NULL
  }
  # get returns the matrix
  get <- function() x
  # setsolve sets the solution
  setsolve <- function(sol) s <<- sol
  # getsolve returns the cached solution (NULL if it wasn't set)
  getsolve <- function() s
  # This function returns a list of the 4 functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cachesolve() gets a "special" matrix, created by makeCacheMatrix(),
## and returns its inversion(if it was previously calculated, it's
## taken from the cache)
cachesolve <- function(x, ...) {
  # If the solution is cached, fetch it using getsolve and return it
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  # Otherwise, fetch the matrix, calculate the solution, save it to the
  # cache and return it
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}