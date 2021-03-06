## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix accepts a matrix and caches the data as required
## The function is driven by another function according to demand.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is the function that accepts the input
## checks in the cache and retrieves the appropriate answer
## The executions are triggered from this function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
        ## Return a matrix that is the inverse of 'x'
}
