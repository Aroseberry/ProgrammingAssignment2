## ARL, 20170226
## R Programming, Assignment #2

## General description
## This file contains two functions, which combine
## to create and cache the inverse of a square matrix

## This function creates a list of four functions
##       set is a function to cache a matrix
##       get is a function to retrieve a cached matrix
##       setinverse is a function to cache the matrix inverse
##       getinverse is a function to retrieve the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function checks for the existence of the cached inverse of
## the matrix input. If it doesn't exist, invert the matrix, cache
## it and return the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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