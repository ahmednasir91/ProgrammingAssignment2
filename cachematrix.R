## Allows caching the inverse of matrix, since calculating inverse of matrix
## is a costly computation, stores the matrix inverse in a variable so 
## it doesn't have to be calculated everytime.

## Creates a matrix which has an ability to cache its inverse, 
## exposes a list of functions that can be used to get, set
## the matrix and its inverse.
## Note: Resets the inverse, when the matrix is updated using set function.
makeCacheMatrix <- function(x = matrix()) {
  # initially set the cached value to NULL
  inv <- NULL

  # Set the matrix
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get Matrix
  get <- function () x
  
  # Set Inverse
  setInv <- function (inverse) inv <<- inverse
  
  # Get Inverse
  getInv <- function () inv
  
  # Return list of functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Returns a matrix that is the inverse of 'x',
## The result is cached, so after calculating it once,
## if the matrix is not changed.
## Note: This function assumes matrix is inversible.
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("Returning cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
