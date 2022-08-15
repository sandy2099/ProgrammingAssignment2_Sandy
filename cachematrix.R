## makeCacheMatrix creates a matrix object that can cache its inverse
## and cacheSolve computes inverse of the matrix returned by the
## makeCacheMatrix. If inverse is already computed then CacheSolve will
## retrieve the inverse from cache. Caching is helpful when the computation
## is time consuming.

## The makeCacheMatrix function creates an object to store a matrix and
## caches its inverse. It creates a special matrix which is a list 
## containing a function to 1. set the value of a matrix, 2. get the value of
## a matrix, 3. set the value of the inverse and 4. get the value of the 
## inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse is already been computed and it has not changed,
## then cacheSolve will retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
