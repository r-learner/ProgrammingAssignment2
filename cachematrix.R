## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The function, makeCachematrix creates a special "vector", which returns
# a list of functions to
# 1. set the value of the vector to the matrix
# 2. get the value of the vector for the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }

  get <- function() x
  setInverse <- function(result) invMatrix <<- result
  getInverse <- function() invMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# The function cacheSolve calculates the inverse of the special "vector" created
# using the function makeCacheMatrix.
# It will check whether the inverse of that matrix is cached, if so, the cached
# inverse is fetched from list, else computes the inverse and save the result
# to cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
  invMatrix
}
