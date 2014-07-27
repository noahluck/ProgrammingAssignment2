## The makeCacheMatrix() and cacheSolve() are designed to find
## and cache the matrix inverse once instead of potentially many
## times, which may save time for large matrices.

## This is a modified version of the example function makeVector()
## where the vector has been replaced with a matrix and the mean
## has been replaced with a matrix inverse.  With an input matrix,
## makeCacheMatrix() returns an list of functions for (re)setting
## matrix, getting its value, getting its inverse, or setting a 
## cached copy of its inverse to save time.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This is a modified version of the example function cachemean()
## where the vector has been replaced with a matrix and the mean
## has been replaced with a matrix inverse.  Given a matrix object
## made by makeCacheMatrix(), cacheSolve() checks if the inverse
## has been found, returns it if it has, or calculates, sets, and
## returns it if it has not.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
