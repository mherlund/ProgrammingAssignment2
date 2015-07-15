## Functions to create a special cached matrix object [makeCacheMatrix] along with a function [cacheSolve]
## to find the inverse matrix. 

## Function to create an object for holding the matrix and inverse when computed.
## To use store the returned object in a var like: myCacheMatrix<-makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
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


## Function to compute the inverse of the matrix returned from makeCacheMatrix, or retreive from cache if already solved.
## Use: cacheSolve(myCacheMatrix)
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
