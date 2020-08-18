## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix creates a matrix
## sets the values of the matrix
## gets the x value from the function
## sets the inverse of the matrix
## gets the inverse from the function
## y is a free variable

makeCacheMatrix <- function(x = matrix()) {
  function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
      x <<- y
      invMat <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) invMat <<- solve
    getInverse <- function() invMat
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
}


## takes the makeCacheMatrix as argument
## if the inverse is calculated it grabs it
## if the inverse was not found, its grabs x
## solves for the inverse
## changes setInverse to invMat
## returns invMat

cacheSolve <- function(x, ...) {function(x, ...) {
  invMat <- x$getsolve
  if(!is.null(invMat)) {
    message("getting cached data")
    return(invMat)
  }
  data <- x$get()
  invMat <- solve(data, ...)
  x$setInverse(invMat)
  invMat
}
        ## Return a matrix that is the inverse of 'x'
}
