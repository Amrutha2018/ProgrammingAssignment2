## Put comments here that give an overall description of what your
## functions do




# makeCacheMatrix creates a special “matrix”, 
# which is really a list containing a functions
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  # get the value of the inverse
  getinverse <- function() i
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}








# This function computes the inverse of the 
# special “matrix” returned by makeCacheMatrix above. 
# If the inverse has already been calculated 
# (and the matrix has not changed),
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  # return the inverse if its already set
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  # Calculate the inverse using matrix multiplication
  i <- solve(data, ...)
  
  # Set the inverse to the object
  x$setinverse(i)
  
  # Return the matrix
  i
}
