## This is an exercise to apply the usage of different R functionalities.
## In this assigment, our goal is to code two different functions, whose 
## objective is to cache the Matrix Inverse operation.

## This function creates a special Matrix that saves the inverse value in
## cache for the matrix that is entered.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
              x <<- y
              im <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {im}
        list(set = set, get = get, setInverse = setInverse, getInverse = 
               getInverse)
        
  }


## This functions uses the data obtained in the previous function to return
## the "cache'd" inverse matrix value. This function will only recover the
## value previously saved.

cacheSolve <- function(x, ...) {
      im <- x$getInverse()
      if(!is.null(im)){
        message("Cached data output...")
        return(im)
      }
      mtx <- x$get()
      im <- solve(mtx, ...)
      x$setInverse(im)
      im
}
