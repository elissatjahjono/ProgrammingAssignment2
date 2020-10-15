## The functions work to store a matrix input, solve the matrix, and cache the resulting inverse matrix.


## This is the first function. It makes a 'special' matrix object.
## This function contains list of functions to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This is the second function. It generates the inverse or 'solve' the input matrix from the first function.
## If the inverse already existed, this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


## To test whether the functions above work

mat <- matrix(1:4, nrow = 2, ncol = 2) ## an example of an invertible matrix
x <- makeCacheMatrix(mat) ## run the first function
cacheSolve(x) ## run the second function to return the inverse
cacheSolve(x) ## run the second function again to retrieve cached data
