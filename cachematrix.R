## The goal of these functions is to return the inverse of a matrix, with the
## assumption that it is invertable, only if this is the first time for this 
## specific ooperation.

## The first funtion makes a global matrix object, and has functions for
## getting and setting the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a matrix object from makeCacheMatrix, and returns the
## inverse matrix, solving for it if it is the first run, and returning the
## saved (cached) result if it is a subsequent operation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
