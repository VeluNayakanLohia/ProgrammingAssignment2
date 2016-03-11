## We first create a function (makeCacheMatrix) which takes the matrix, stores it,
## check for it's inverse, gets that value.
## The second function (cacheSolve) does the background work of finding and
## keeping the inverse in cache.

## This is the main function which creates
## a special "matrix" who's inverse is to be determined.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function creates the inverse of the matrix if its not there by
## calling the functions like getinverse, etc.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Returns a matrix that is the inverse of 'x'
}
