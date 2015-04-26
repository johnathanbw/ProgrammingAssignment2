## This file contains two main functions:
##
## 1. makeCacheMatrix
## 2. cacheSolve
##
## The primary purpose of these functions are to
## cache or store the inverse of the matrix that
## is being input, so that there is no need to
## calculate the inverse repeatedly if it has
## already been calculated before.

#######################
## makeCacheMatrix creates a special "matrix" 
## object that can cache its inverse.
## Its output is a list with four functions:
## 1. "set", which sets the value of the vector
## 2. "get", which gets the value of the vector
## 3. "setinv", which sets the inverse of the matrix
## 4. "getinv", which gets the value of the matrix
#######################

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
  
}


## cacheSolve calculates the inverse of the special
## "matrix" created with makeCacheMatrix.
##
## It first checks to see if the inverse has already
## been calculated. If so,  it gets the inverse from the 
## cache and skips the computation. 
##
## Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via 
## the setinv function.

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

mat <- matrix(c(5,2,2,5),nrow=2,ncol=2)
inve <- makeCacheMatrix(mat)
inve2 <- cacheSolve(inve)