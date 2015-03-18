## Coursera: R Language
## Assignment #2
## Purpose of this file: Write 2 functions that cache the inverse of a matrix

## makeCacheMatrix function creates a special matrix object that
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solv) m <<- solv
        getinverse <- function() m

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated
## and the matrix has not changed, then this function will retrieve the
## inverse from the cache
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
