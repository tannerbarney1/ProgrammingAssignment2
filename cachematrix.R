## The overall purpose of the functions is to return the inverse of a matrix and to do so from cache if available
## Creating inverses of matrices can be demanding so this function saves compute time by using data stored in cache
## when available

## makeCasheMatrix creates a list with four functions to get and set the value of the input matrix and
## as well as the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve returns the inverse of a matrix either from cache if available or computes the inverse if not.
## cacheSolve requires as an argument an object that has been created through makeCacheMatrix

cacheSolve <- function(x, ...) {

  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
