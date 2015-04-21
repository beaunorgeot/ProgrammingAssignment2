
# Script contains a 2 function process that sets, caches, and gets the inverse of a matrix
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.

# R really needs to support a docString option, this is rediculous

#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Return a matrix that is the inverse of 'x' by calculating the solve/inverse of the special "matrix" created with the above function. 
# 1.It first checks to see if the inverse has already been calculated. 
# 2.If so, it gets the inverse from the cache and skips the computation. 
# 3.Otherwise, it calculates the inverse of the matrix and sets the value of the mean in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
