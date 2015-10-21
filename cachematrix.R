## This file contains the functions for the Programming Assignment 2
## for the Coursera R Programming class
##
## It contains 2 functions:
##  makeCacheMatric which creates a matrix object which can cache it's inverse
##  cacheSolve which computes the inverse of the matrix and stores it into the matrix object
##
##  Note that it is important that only the gets and sets are used for the object as forcing in a matrix
##  to the object will not clear a cached inverse.



## The makeCachMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                 ## initializes the inverse matrix to null
  set <- function(y) {      ## set function which sets the matrix and initializes the inverse to null
    x <<- y
    m <<- NULL
  }
  get <- function() x       ## get function returns the matrix
  setinvmatrix <- function(inv) m <<- inv     ## set function for the inverse
  getinvmatrix <- function() m                ## get function for the inverse
  list(set = set, get = get,                  ## list of functions for this object
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()                 ## check if the inverse is already cached and return it if true
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()                       ## inverse is not cached so calculate it and store it in the object
  m <- solve(data, ...)                 ## and return the newly calculated inverse
  x$setinvmatrix(m)
  m
}
