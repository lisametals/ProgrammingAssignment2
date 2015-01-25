## This file contains a set of functions that cache the result of the computation
## and then uses the cached computation (when it is available) instead of calculating 
##  the inverse of a matrix from  scratch each time

## makeCacheMatrix first sets the value of the matrix,  and then gets the value of the matrix

makeCacheMatrix <- function(inputmatrix = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    inputmatrix <<- y
    inverse <<- NULL
    }
get <- function() inputmatrix
setinv <- function(solve) inverse <<- solve
getinv <- function() inverse
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
     
}



## cacheSolve checks if inverse of the matrix has already beeen cached, and if yes, returns
## the cached matrix. If it has not been cached, it calculates the inverse using the solve function


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
   if(!is.null(m)) {
     message("getting cached data")
    return(m)
    }
  data <- x$get()
  
  # if result not cached, use solve to calculate  inverse of matrix
  m <- solve(data)
    x$setinv(m)
  m
}
  
