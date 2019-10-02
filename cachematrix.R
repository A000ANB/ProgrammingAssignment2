## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates on object containing inversed matrix of 
## the initial given matrix
## cacheSOlve supposed to get an object of makeCacheMatrix and return
## inversed matrix stored in the object

## Write a short comment describing this function
## makeCacheMatrix caches given matrix and on request setInverse it stores 
## in cache inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function() invMatrix <<- solve(x)
  getInverse <- function() invMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve returns a matrix inverted from one given in an object as argument
## It checks if inverted is already cached, then returns it
## If inverted was not cached, it calls methods of the object to invert
## And returns inverted matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix))
  {
    message("retrieving chached inverse matrix")
    return(invMatrix)
  }
  x$setInverse()
  x$getInverse()
}