## These functions are used for storage of a matrix and it's inverse
## so it could be used in computation multiple times.

## A function that stores a matrix and it's inverse
## if it's already calculated.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  getMatrix <- function() x
  setMatrix <- function(y){
    x <<- y
    i <<- NULL
  }
  getInverse <- function() i
  setInverse <- function(inv) i <<- inv
  list(getMatrix = getMatrix, getInverse = getInverse, 
       setMatrix = setMatrix, setInverse = setInverse)
}


## This function returns the inverse of the matrix.
## If it's not yet calculated, the function calculates it first.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("Getting cached inverse")
    return(inv)
  }
  matr <- x$getMatrix()*1.0
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}
