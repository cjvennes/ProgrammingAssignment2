## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special martix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  ##create the inv of null to ensure variable is clear of data
  inv <- NULL
  
  ##cache the inputted matrix
  set <-function(y){
      x <<- y
      inv <<- Null ##if there is a perviously saved inverse, clear that from the cache
  }
  
  ##return the cached matrix
  get <- function() x
  
  ##set the inverse of the given matrix using the solve function and saves to cache
  setinv <- function(solve) inv <<- solve
  
  ##returns the cached inverse
  getinv <- function() inv
  
  ##returns list of functions that can be called later
  list (set= set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve computes the inverse of the special matrix returned by makeCache above.
  ##If the inverse is already been calculated (and the matrix has not changed) then cacheSolve
  ##should etreieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ##for the inputted matrix, pull the cached inverse
  inv <- x$getinv()
  
  ##check if there is a cached inverse
  if(!is.na(inv)){
      ##If there is a cached inverse, return that and let the user know you pulled the cached data
      message("getting cached data")
      return(inv) ##if there is a cached inverse return it and stop the function
  }
  
  ##if there isn't a cached inverse, create a new inverse
  data <- x$get() ##get the inputted matrix
  inv <- solve(data, ...) ##create the inverse of the matrix
  x$setinv(inv) ##set the matrix to the cache
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
