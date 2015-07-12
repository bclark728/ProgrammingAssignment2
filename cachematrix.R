################################################################################### 
## cachematrix.R                                                                 ##
#   Methods for creating a cacheable matrix, calculating and caching its inverse  #
#    and retrieving the inverse from memory.                                      #
###################################################################################


## makeCacheMatrix(trix)
#   Returns a list of four functions to set and get the value of a cacheable matrix
#    and its inverse. Caches both the matrix and its inverse.

makeCacheMatrix <- function(trix = matrix()) {
  # By default, trix.inv is set to NULL
  trix.inv <- NULL
  
  # set() caches the matrix, and resets the inverse to NULL, since it hasn't been
  #  calculated yet
  set <- function(y) {
    trix     <<- y
    trix.inv <<- NULL
  }
  
  # get() retrieves the matrix from cache
  get <- function() trix
  
  # setinv() caches the inverse
  setinv <- function(inv) trix.inv <<- inv
  
  # getinv() retreives the inverse from cache
  getinv <- function() trix.inv
  
  # Return the access functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve(x, ...)
#   Get the inverse of the matrix accessed through function list x. Other arguments
#    get passed to solve(). If this is the first time calculating the inverse
#    cacheSolve() calculates the inverse, caches it and returns it. If it has been
#    calculated previously, the inverse is returned from cache.

cacheSolve <- function(x, ...) {
  # Pull the inverse from cache
  inv <- x$getinv()
  
  # If it isn't NULL, then the inverse has been calculated. Return it from cache.
  if(!is.null(inv)) {
    print("using cached")
    return(inv)
  }
  
  # Otherwise, pull the matrix from cache and calculate the inverse
  trix <- x$get()
  inv  <- solve(trix, ...)
  
  # Cache the inverse and return it
  x$setinv(inv)
  inv
}
