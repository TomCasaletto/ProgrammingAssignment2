## These functions work together to create a matrix object and
## cache its inverse.

## The following function is an object encapsulating a matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## The following function works with the object above to cache the
## matrix inverse upon first invocation.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        ## Have matrix, return the cached inverse.
        message("getting cached data")
        return(i)
    }

    ## First time called with this matrix.
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
