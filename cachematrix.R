## Two functions that cache the inverse of a matrix.

## The makeCacheMatrix function takes a matrix and creates a special matrix
## object. When called it returns a list that includes four named elements 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function computes (if not done already) the inverse of the
## cached matrix and is returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting the cached data")
    return(m)
  }
  cdata <- x$get()
  m <- solve(cdata, ...)
  x$setinverse(m)
  m
}