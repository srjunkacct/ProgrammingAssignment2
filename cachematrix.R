## this source file defines functions makeCacheMatrix
## and cacheSolve that provide the ability to cache
## the inverse of a matrix.

## makeCacheMatrix defines a matrix caching object that
## holds the matrix x as well as its inverse if it has
## already been computed.
## it defines the methods
## set(m=matrix()) to re-set the value of the matrix
## get() to retrieve the value of the matrix
## setinverse(inv=matrix()) to set the value of the inverse
## getinverse() to retrieve the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  xinverse <- NULL
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  get <- function() { return(x) }
  setinverse <- function(inv) { xinverse <<- inv }
  getinverse <- function() { return(xinverse) }
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve will first check if the inverse for x has
## already been computed and cached, and return the
## cached value if this is the case.  Otherwise it
## computed and caches the inverse of x.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinverse(inv)
  return(inv)
}

