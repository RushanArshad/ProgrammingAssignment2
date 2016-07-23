

## This function creates a matrix that needs to be used at a later time and must be cached to decrease computation

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


##This function computes the inverse of a matrix that needs to be cached and it checks first if the inverse of the matrix is already computed ot not and then acts accordingly.

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
