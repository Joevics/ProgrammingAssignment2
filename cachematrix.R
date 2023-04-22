## These two functions create a special object that stores a matrix and caches its 
##inverse.


## makeCacheMatrix: This function creates a unique “matrix” which is a list containing a 
##function to: set the value of the matrix, get the value of the matrix, set 
##the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  mc <- NULL
  set <- function(y) {
          x <<- y
          mc <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mc <<- inverse
  getinverse <- function() mc
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned by 
##makeCacheMatrix

cacheSolve <- function(x, ...) {
  mc <- x$getinverse()
  if (!is.null(mc)) {
    message("getting cached data")
    return(mc)
  }
  data <- x$get()
  mc <- solve(data, ...)
  x$setinverse(mc)
  mc
}