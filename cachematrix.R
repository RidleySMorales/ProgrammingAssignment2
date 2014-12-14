## cache the given matrix
makeCacheMatrix <- function(x = matrix()) {
     ## set variables
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     ##get the data
     get <- function() x
     ##store the solution in m 
     setsolve <- function(solve) m <<- solve
     ##get the solution
     getsolve <- function() m
     ##create the list
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
     ## return the answer if it was calculated previously
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     ##calculate the inverse
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
