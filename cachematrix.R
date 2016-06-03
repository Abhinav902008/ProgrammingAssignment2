## Matrix inversion is usually a costly computation and hence there are benefits to cache the 
## inverse of a matrix rather than compute it repeatedly. The following pair of functions 
## caches the inverse of a matrix

########################## 1. First Function ############################
##  This function creates a special "matrix" object that can cache its inverse

## The first function, makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseMatrix) inverse <<- inverseMatrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

######################### 2. Second Function ###########################

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache

# It is assumed that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
  
}
