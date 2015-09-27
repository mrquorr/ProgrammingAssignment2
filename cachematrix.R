## functions to store a matrix in cache as well as its inverse so as to not calculate
## the inverse every time


## returns a list that is really a set of functions for interacting with the matrix
##1 set the value of the matrix
##2 get the value of the matrix
##3 set the value of the inverse
##4 get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
	changed <<- 1
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    getchanged <- function() changed
    setchanged <- function(c) changed <<- c
    list(set = set, get = get, setinverse = set inverse,
         getinverse = getinverse)
}

## attempts to return the cached inverse of the matrix, if it is not found or if
## the matrix has been changed it is calculated and stored in cache

cacheSolve <- function(x, ...){
    m <- x$getinverse()
    changed <- x$getchanged()
    if(!is.null(m) || changed == 0) {
        message("getting cached data")
        return(m)
    }
    x$setchanged(0)
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
