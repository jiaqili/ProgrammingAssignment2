## Put comments here that give an overall description of what your
## functions do

## Making a cache matrix

## Given an inversible matrix, returns a list containing 4 function objects
## 1. set function to set a variable as a value and set the cached inverse 
##    as NULL
## 2. get function to return the value of a variable
## 3. setInverse function to set the value of the cached inverse variable
## 4. getInverse function to return the value of the cached inverse variable
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Return a matrix that is the inverse of 'x'

## Pass in a list returned in makeCacheMatrix function. If the value of the 
## inverse matrix has never been computed before, compute the inverse and 
## return it.
## If the inverse has been computed before, return the cached value from the
## inverse variable rather than computing the inverse again
cacheSolve <- function(x, ...) {
  ## Get the cached value of the inverse matrix
  inverse <- x$getInverse()
  
  ## If the cached inverse matrix is not null, then it has been calculated 
  ## before. Return the cached inverse and exit the function
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  ## If the inverse variable is null then solve for the inverse matrix and 
  ## return the result
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}