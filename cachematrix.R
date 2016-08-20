#################################################################################################
#################################################################################################
#
# The following two functions named makeCacheMatrix and cacheSolve
# makeCacheMatrix function creates a special "matrix" object that can cache its inverse and 
# cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# function retrieves the inverse from the cache.
#
#
#
# first function: makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) {
            inverse <<- inverse
      }
      getinverse <- function() {
            inverse
      } 
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#
#
# second function: cacheSolve


cacheSolve <- function(x, ...) {
      
      inverse <- x$getInverse()
      if (!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      matrix <- x$get()
      inverse <- solve(matrix, ...)
      x$setInverse(inverse)
      inverse
}