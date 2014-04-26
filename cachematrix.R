## These functions will cache the inverse of a matrix
## The cache will be use to prevent repeat computations


## makeCacheMatrix stored the value of the matrix
## and it's inverse i.e does the caching

makeCacheMatrix <- function(x = matrix()) {
  # clear m; m will be computed with cacheSolve
  m <- NULL
  # calling set globally set x to contain y passed into it
  # m is globally set to NULL (else scoping affected)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # returns input value x
  get <- function() x
  # this is the cached value
  setinverse <- function(solve) m <<- solve
  # return the cached value
  getinverse <- function() m
  # return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve retrieves the cached value of the inverse
## if exists, else computes it

cacheSolve <- function(x, ...) {
  # get inverse from special cache matrix
  m <- x$getinverse()
  # if exists retrieve it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #get data from special cache matric
  data <- x$get()
  #compute the inverse
  m <- solve(data, ...)
  # store inverse value in cache
  x$setinverse(m)
  # return inverse
  m
}
