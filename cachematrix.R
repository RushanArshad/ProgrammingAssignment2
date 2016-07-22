## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          setmat <- function(y)
          {
            x <<- y
            inv <- NULL
          }
          getmat <- function()
          {
            x
          }
          setInverse <- function(inverse)
          {
            inv <<- inverse
          }
          getInverse <- function()
          {
            inv
          }
          list(setmat = setmat,getmat = getmat,setInverse = setInverse,getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
          inv = x$getInverse
          if(!is.null(inv))
          {
            message("Getting Cached Inverse of the Matrix")
            return(inv)
          }
          matdata <- x$getmat()
          inv <- solve(matdata,...)
          x$setInverse(inv)
          inv
  ## Return a matrix that is the inverse of 'x'
}
