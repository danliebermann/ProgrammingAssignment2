## Two functions 1) makeCacheMatrix creates a special vector containg
## four functions for getting/setting a matrix and getting/setting its inverse.
## 2) cacheSolve return the inverse of a matrix using a cached version if
## available.

## Creates a special vector containing four functions

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

## Returns the inverse of a matrix using a cached version if available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinverse(i)
  i
}
