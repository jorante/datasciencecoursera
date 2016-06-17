## Course : Coursera - R Programming
## Programming Assignment 2


## makeCacheMatrix - function creates a matrix and sets inverse calculation in cache

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  get <- function() {
    x
  }
  set <- function(newMatrix) {
    x <<- newMatrix
    inverseM <<- NULL
  }
  getInverse <- function() {
    inverseM
  }
  setInverse <- function(solvedMatrix) {
    inverseM <<- solvedMatrix
  }
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## function cacheSolve

cacheSolve <- function(x, ...) {
  inverseM <- x$getInverse()
 
  if(!is.null(inverseM)) {
    return(inverseM)
  }
  
  data <- x$get()
  inverseM <- solve(data, ...)
  x$setInverse(inverseM)
  
  return(inverseM)
}
