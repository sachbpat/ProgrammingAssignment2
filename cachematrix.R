## These functions cache the inverse of a matrix and if the same matrix 
## required to be inversed repeatatively, it is directly taken from the cache

## The function creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## Initiate a matrix to assign inversed matrix

 s <- NULL

##  Function to set the values to a matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }

## Function to get the values a matrix
        get <- function() x

## Function to inverse a matrix and store in cache
        setinv <- function(solve) s <<- solve

## Function to get inversed matrix
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s

}
