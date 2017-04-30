## The function takes a matrix, calculate inverse of it after retrieving the matrix from the cache list, gives the original function and the inverse of it.

## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  my_invrs <- NULL
  set <- function(y) {
    x <<- y
    my_invrs <<- NULL
  }
  get <- function() x
  setinvrs <- function(inverse) my_invrs <<- inverse
  getinvrs <- function() my_invrs
  list(set = set, get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
}



## cacheSolve

cacheSolve <- function(x, ...) {
  my_invrs <- x$getinvrs()
  if(!is.null(my_invrs)) {
    message("getting cached data")
    return(my_invrs)
  }
  data <- x$get()
  my_invrs <- solve(data, ...)
  x$setinvrs(my_invrs)
  my_invrs
}

