## Pair of functions that cache the inverse for square invertable matrix.

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   inverse <- NULL
   setMatrix <- function(y) {
      matrix <<- y
      inverse <<- NULL
   }
   getMatrix <- function() x
   setInverse <- function(invert) inverse<<-invert
   getInverse <- function() inverse
   
   return(list(setMatrix = setMatrix
               , getInverse = getInverse
               , setInverse = setInverse
               , getMatrix = getMatrix ))
}


##Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
##If the inverse has already been calculated por particular matrix,
##then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   inverse <- x$getInverse()
   if(!is.null(inverse))
   {
      message("catched inverse")
      return(inverse)
   }
   data <- x$getMatrix()
   inverse <- solve(data)
   x$setInverse(inverse)
   inverse
}