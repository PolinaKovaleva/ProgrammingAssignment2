## These two functions create get the inverse matrix for matrix x:
## from cached memory if it was previously calculated
## calculate anew and saves in cache if it was not done before
## in the global environment


## This function creates a list of functions that serve to 
## set up and call the cached value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) z <<- solve
  getinv <- function() z
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function gets the inverse matrix of x cached in the environment 
# if available, or directly calculates the inverse of the matrix otherwise

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getinv()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinv(z)
  z
}
