
## This function read in a matrix and creates a set of functions for the matrix manipulation into Cache and out of Cache
## A function list object is generated to be used by other function e.g. mat<-makeCacheMatrix(x)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  
  ##Generate the return as a list object.
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
## This function get the List object as the input and do the inverse opeartion for the matrix
## e.g. CacheSolve(mat)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
