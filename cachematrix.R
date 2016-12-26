## System to cache the inverse value of a matrix to be accessed without recalculating

## This function (makeCacheMatrix) creates the cache to be accessed by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function (cacheSolve) returns the inverse of the matrix from 
## cache or calculates if needed

cacheSolve <- function(x, ...) {    
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)                         
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)                              
  i                    
}
