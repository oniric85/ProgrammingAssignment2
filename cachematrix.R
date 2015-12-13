## These functions allow user to build a special version of a matrix object which can
## be used to cache its inverse. This allows us to reduce computation instead of always
## recalculating the inverse of the same matrix.

## This function returns a list of functions which operates on the closing environment.
## This allows us to create a mutable state that can be used to cache computationally intensive
## data.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) i <<- inverse

        getinverse <- function() i
  
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## This function is a version of the solve() R function which takes advantage of the 
## makeCacheMatrix result and only really calculate the inverse if it was not calculated before
## for the same matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

        data <- x$get()

        i <- solve(data, ...)
        x$setinverse(i)

        i
}
