## Matrix inversion is usually a costly computation and so there may be some
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The following two functions can be used to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
# It creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function (x = matrix()) {
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function () x
  setmatrix <- function (solve) m <<- solve
  getmatrix <- function () m
  list (set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


##  This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix above. It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. If not, it computes 
# the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function (x = matrix (), ...) {
  m <- x$getmatrix ()
  if (!is.null (m)){
    message ("getting cached data")
    return (m)
  }
  matrix <- x$get ()
  m <- solve (matrix, ...)
  x$setmatrix (m)
  m
}
