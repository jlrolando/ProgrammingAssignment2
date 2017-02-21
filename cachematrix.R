## This set of functions caches the inverse of a matrix, if it has been operated before
## This procedure allows to reduce the computation needed to return a value runed before


## makeCacheMatrix creates a fake "Matrix", containing a list of functions... 
## ... to set and get the value of a matrix, and to set an get the inverse value of the matrix.
## ....The output of this function will be run in the cacheSolve function (see below):
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse of 'x'. 
## makeCacheMatrix arguments should be inserted in this function
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}