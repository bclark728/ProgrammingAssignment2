## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(trix = matrix()) {
  trix.inv <- NULL
  set <- function(y) {
    tirx     <<- y
    trix.inv <<- NULL
  }
  get <- function() trix
  setinv <- function(inv) trx.inv <<- inv
  getinv <- function() trix.inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    print("using cached")
    return(inv)
  }
  trix <- x$get()
  inv <- solve(trix, ...)
  x$setinv(inv)
  inv
}
