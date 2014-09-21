## These functions are used to create a matrix object that
## is capable of caching it's inverse for faster future 
## use and access. 

## Author: Matt Allen
## Created On: 9/21/2014
## For Coursera R Programming

## This function creates a special matrix that is 
## able to cache its inverse.
##
## Functions:
##   get() : Returns the matrix
##   set(x): Sets the value of the matrix and inits the inverse to NULL
##   getinverse()  : Returns the inverse of the matrix, calculating it
##                 : and caching it if necessary.
## 
## Example:
## > z <- matrix(1:4,2)
## > z$getinverse()
## getting cached inverse matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## This function sets the value of the matrix
  ## and inits the value of the inverse to NULL.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## This function returns the matrix.
  get <- function() x
  
  ## Calculating the inverse of a matrix is done by swapping
  ## the a and d positions in a matrix, multiplying the b and c
  ## positions by negative 1, and then dividing everything by
  ## the determinent (ad-bc). This function accomplishes the 
  ## calculation by calling the R solve method and caching the 
  ## result. If the function has already been called for a given
  ## x, the cached value is returned.
  getinverse <- function() {
    if(!is.null(i)) {
      message("getting cached inverse matrix")
      return(i)
    }
    i <<- solve(x)
    i
  }
  
  ## Creates a list of functions to access the 
  ## special matrix object.
  list(set = set, get = get,
       getinverse = getinverse)
}


## This function is a convenience method to get the 
## value of the inverse matrix for x. It calls
## the getinverse method on x which, in turn, handles
## calculating the inverse and returning it (if necessary)
## or simply returning the cached value.
##
## Example:
## > z <- matrix(1:4,2)
## > cachesolve(z)
cacheSolve <- function(x) {
  i <- x$getinverse()
  i
}