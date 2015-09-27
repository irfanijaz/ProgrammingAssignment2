## purpose of the 2 functions is to check if the inverse of the matrix is already calculated and present in the cache, if
## so pick from the cache else calculate it

## purpose of this function is to create a set of 4 functions to be used in conjunction with the 2nd function in
## evaluating if inverse of the matrix is already present in the cache

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

## the function calculates the inverse of the matrix if it not found in the cache.

cachemean <- function(x, ...) {
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
