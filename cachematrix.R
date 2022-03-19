## The first function is able to create an object to store a matrix inverse and 
##the second function takes the matrix and returns its inverse from the cache 
##if it's already stored, or calculates the inverse, stores it and returns it.

## This function creates the special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) i <<- invert
  getinvert <- function() i
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## This function returns the inverse matrix already calculated or calculates it 
##and returns it.

cacheSolve <- function(x, ...) {
  i <- x$getinvert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinvert(i)
  i
}