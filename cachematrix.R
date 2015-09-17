# Overview of functionality provided --------------------------------------

# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly. This pair of functions cache the inverse of a matrix.
#
# `makeCacheMatrix` creates a special "matrix" object that can cache
# its inverse. `cacheSolve` computes the inverse of the special "matrix"
# returned by `makeCacheMatrix`. If the inverse has already been
# calculated and the matrix has not changed), then `cacheSolve`
# retrieves the inverse from the cache.
#
# The matrix supplied must be invertible.



# The `makeCacheMatrix()` function ----------------------------------------

# # Creates a special "matrix" object that can cache its inverse.
#
# # Arguments:
#
# # x: a matrix that can be inverted
#
# # Example:
#
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# h8 <- makeCacheMatrix(hilbert(8))
#
# # View the values:
#
# h8$get()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  solveinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, solveinv = solveinv, getinv = getinv)
}


# The `cacheSolve` function -----------------------------------------------

# # Computes the inverse of the special matrix created by `makeCacheMatrix()`
#
# # Arguments:
#
# #   x: A special matrix created using `makeCacheMatrix()`
# # ...: Other arguments to `solve()`
#
# # Example:
#
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# h8 <- makeCacheMatrix(hilbert(8))
# cacheSolve(h8)
# cacheSolve(h8) ## <---- This would return the cached value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$solveinv(m)
  m
}
