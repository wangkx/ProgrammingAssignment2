## This R script file contains two functions: makeCacheMatrix and 
## cacheSolve. The makeCacheMatrix function accepts a matrix as input 
## and caches the matrix into cache. The cached matrix can be used by 
## the cacheSolve function which calculates the inverse of the cached 
## matrix and caches the inverse for future use.

## The makeCacheMatrix function caches the matrix data x and resets 
## the inv to NULL It also creates four nested functions (set, get, 
## setinverse, and getinverse). The set function caches the new 
## matrix data y to the x and resets the inv to NULL. The get function 
## returns the cached matrix data x. The setinverse function is used 
## to cache the inverse of the matrix. The getinverse function returns 
## the cached inverse or, if no inverse has not been set yet, returns 
## NULL.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve function accepts the makeCacheMatrix function as input.
## At first, it trys to read cached inverse from the makeCacheMatrix. If 
## the cached inverse is found (not NULL), the cacheSolve function shows
## a message "getting cached data" and returns the cached inverse. If 
## the cached inverse is not found, the function reads the cached matrix 
## data by calling makeCacheMatrix$get(), calculates the inverse of the 
## cached matrix using slove(), and caches the inverse. At the end, it 
## returns the inverse.

cacheSolve <- function(x, ...) {
    int <- x$getinverse()
    if(!is.null(int)) {
        message("getting cached data")
        return(int)
    }
    data <- x$get()
    int <- solve(data, ...)
    x$setinverse(int)
    int
}
