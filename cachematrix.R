## R Programming 
## Programming Assignment 2
## Heavily based on the Caching the mean of a vector example
## The assignment is to write a pair of functions 
##     that cache the inverse of a matrix.

## For this assignment, assume that the matrix supplied is always invertible.

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  #set new matrix
  set <- function(y) {
    # the matrix object
    x <<- y 
    #reset the inverse
    inv <<- NULL
  }
  
  #return the matrix object
  get <- function() x
    
  #set inverse
  setinv <- function(newInv) inv <<- newInv
  
  #return inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  
  #check if inverse hasn't been calculated before
  if (is.null(x$getinv())) {
    
    ## For this assignment, assume that the matrix supplied is always invertible.
    ## inv <- solve(x$get())
    ## x$setinv(inv)
    
    ##If we don't assume the matrix is invertible
    if (nrow(x$get())==ncol(x$get()) && det(x$get())!=0) {
      inv <- solve(x$get())
      x$setinv(inv)
    }
    else {
      x$setinv(NULL)
      print("Matrix is singular. No inverse.")
    }
  }
  #inverse has already been cached
  #just return the cached value
  inv
}