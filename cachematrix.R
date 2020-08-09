## These functions enable the creation of a matrix and 
## computation of its inverse, caching the inverse to save
## computational time in repetitive tasks

## makeCacheMatrix creates and returns a list containing
## functions to set and retrieve both a matrix object's value
## and its inverse using a 'caching' mechanism

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks to see if a matrix's inverse is 
## cached, if so it will return the inverse, otherwise it will
## compute, cache, and return the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
