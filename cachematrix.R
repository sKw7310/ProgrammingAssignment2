## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
#Setting the matrix function
    set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# Function  checks for if inverse already cached and matrix did not change
# and returns cached value.
# If no inverse was cached or matrix changed then it calculates and returns
# inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
     if(identical(x$get(), solve(m))) {
      message("getting cached data")
    return(m)
  }
  }
# Getting matrix if not cahced or matrix changed  
  data <- x$get()
# Calculating inverse of matrix  
  m <- solve(data, ...)
# Setting inverse of matrix
  x$setinv(m)
  m
}
