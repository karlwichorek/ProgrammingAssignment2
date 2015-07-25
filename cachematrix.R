# The following functions work together to create a square invertible matrix
# and make the inverse of the matrix available in the cache environment

# makeCacheMatrix defines and returns the list of functions
# used by cacheSolve to get or set the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
  # clear cache
  cache <- NULL
  
  # create the matrix in the working environment
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  # get value of the matrix
  get <- function() x
  
  # invert matrix and store result in cache
  setMatrix <- function(inverse) cache <<- inverse
  
  # get inverted matrix from cache
  getInverse <- function() cache
  
  # push functions to working environment
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}

# cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
# If the inverted matrix does not exist in cache,
# it it created in the working environment 
# and it's inverted value is stored in cache
cacheSolve <- function(x, ...) {
  
  # Fetch the cached value for the inverse
  m <- x$getInverse() 
  
  # If the cache is not empty, return cache
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  
  # If the cache was empty, calculate the inverse, store it in the cache, 
  # and then return the result
  
  # Get value of matrix
  data <- x$get()
  
  # Calculate the inverse of the matrix
  m <- solve(data)
  
  # Cache the result
  x$setInverse(m)
  
  # Return the inverse
  return(m)
}