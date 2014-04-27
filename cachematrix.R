## The following function, makeCacheMatrix, creates a special "matrix" object, 
## which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the stored inverse value to NULL
  inv <- NULL 
  # set the value of the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix        
  get <- function() x
  # set the inverse
  setinv <- function(inv_) inv <<- inv_
  # get the inverse
  getinv <- function() inv
  # return a list of all the above functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The following function, cacheSolve, calculates the inverse of the 
## special "matrix" created with the above function. However, it first
## checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the matrix and sets the value of the inverse 
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  # check if the inverse is already cached
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if the inverse is not cached, get the matrix into data
  data <- x$get()
  # and compute the inverse
  inv <- solve(data, ...)
  # then set the value of inverse in cache
  x$setinv(inv)
  # return the value of inverse
  inv
}
