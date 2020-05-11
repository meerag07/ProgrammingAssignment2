## Caching the inverse of a Matrix 

## creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  setMatrix <- function (y) {
      x <<- y
      m <<- NULL 
  }
  getMatrix <- function () x 
  setInverse <- function (solved) m <<- solved
  getInverse <- function () m 
  list (set = setMatrix, getM = getMatrix, setI = setInverse, getI = getInverse)
}


## computes the inverse of the special matrix returned by the above function 

cacheSolve <- function(x, ...) {
  m <- x$getI ()
  if (!is.null(m)){
    message ("getting cached data")
    return (m)
  }
  data <- x$getM
  m <- solve (data, ...)
  x$setInverse (m)
  m 
}
