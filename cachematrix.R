## Functions that cache the inverse of a matrix for time saving
## functions do

## The first function, makeVector creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
 t <- NULL
  set <- function(y) {
    x <<- y
    t <<- NULL
  }
  get <- function() x
  setinv <- function(inv) t <<- inv
  getinv <- function() t
  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


##The following function calculates the mean of the special "matrix" created with the above function. However,
#it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
      t <- x$getinv()
  if(!is.null(t)) {
    message("getting cached data")
    return(t)
  }
  m <- x$get()
  t <- solve(m, ...)
  x$setinv(t)
  t 
}
