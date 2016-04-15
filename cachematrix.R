## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(k) {
    m <<- k
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) 
  i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function

#cacheSolve: A special "matrix" to compute inverse is created by this matrix
#by makeCacheMatrix above. If the inverse has already been calculated (and the
#matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

