## WHY?
## Performance trick if you need to compute the inverse of the same matrix several times (e.g inside loop)
##
## HOW ? 
## `makeCacheMatrix` creates a new object with helper functions that allows to set/get the inverse matrix
## `cacheInverseMatrix` computes the inverse of matrix and retrieve the cached version if it exists
##
## HOW TO USE?
## A user should create a new object from an existing matrix using makeCacheMatrix, and then use cacheInverseMatrix
## to compute the inverse of the matrix
## 
## e.g. > newMatrix <- makeCacheMatrix(mat)
##      > cacheInverseMatrix(newMatrix)


## Return a new object with 4 exposed functions to manipulate a matrix
## - get (return matrix), 
## - set(set new matrix), 
## - setinverse (set the inverse matrix),
## - getinverse (return inverse matrix)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse<- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Return a matrix that is the inverse of 'x'
# It will first retrieve the cached inverse. If the cached inverse is not defined,
# the reverve will be computed and cached for the next call
cacheInverseMatrix <- function(x, ...) {

  inv <- x$getinverse()
  # checks if the inverse was already cached for this object
  if(!is.null(inv)) {
    message("getting cached solved data")
    return(inv)
  }
  # there was no cached inversed, lets compute it and set the cache for next call
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
