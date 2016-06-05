## Put comments here that give an overall description of what your
## functions do
## This function sets a matrix, gets a matrix, set matrix inverse and get matrix
## inverse.  

## Write a short comment describing this function
##If user input is not a matrix or matrix is not square, it produces 
## an error message.

makeCacheMatrix <- function(x = matrix()) {
  if(is.matrix(x)==FALSE ){
    message("Error! Input must be a matrix")}
  else if(ncol(x)!=nrow(x)) {
    message("Error! Input must be a square matrix")
  }
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
## This function returns inverse of the matrix.  
## The function first finds if inverse exists in cache otherwise it calculates
## inverse using solve command.
cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
