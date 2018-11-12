## These two functions implement the possibility to save a cache of the calculation of an inverse matrix

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ## if makeCacheMatrix is already created, the value can be changed with this set function
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      ## function to assign calculated inverse of the matrix to the parent environment
      setInverse <- function(mInverse) m <<- mInverse
      ## function to the inverse of the matrix
      getInverse <- function() m
      ## return and name each element of the list to enable use of the "$" extract operator in the cacheSolve function
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve retrieves the cached inverse of the matrix if already cached, or calculates and stores it in cache  
cacheSolve <- function(x, ...) {
      ## check whether a calculated version is available in cache
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## if not, calculate the inverse matrix and save it in cache
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      ## Return a matrix that is the inverse of 'x'
      m
}
