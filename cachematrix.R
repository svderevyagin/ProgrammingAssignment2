# This file contains two functions:
#   1. 'makeCacheMatrix' creates a special "matrix" object
#   2. 'cacheSolve' returns the inverse of the "matrix" object

# This function creates a special "matrix" object 
# that can cache its inverse
# 
# The "matrix" object consists of 4 functions:
#  1. 'set' - store matrix
#  2. 'get' - get matrix
#  3. 'setinverse' - store inverse matrix
#  4. 'getinverse' - get inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, 
       setinverse=setinv, 
       getinverse=getinv)
}

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then 
# cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
