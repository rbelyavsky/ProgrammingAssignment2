## The following functions create a special object which stores
## a numeric matrix and caches it's inverse.

## makeCacheMatrix takes a numeric matrix as input and creates a 
## special list containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cacheSolve takes the output of makeCacheMatrix and first
## checks if the inverse has already been calculated. 
## If the inverse has been previously calculated, it gets
## the inverse from the cache (returns a message) and skips
## the computation. Otherwise, it calculates the inverse
## and stores it in the cache.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
