## Inverting a matrix is an expensive operation.  As an alternative, hide
## the matrix behind a set of functions which calculates the inverse only
## when necessary and otherwise retrieves a cached copy.

## Generate a list of functions to serve as a "front" for the matrix, x, 
## allowing it to be accessed via a cache.

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


## Solve for the inverse of a matrix via its "front" functions.  Only
## calculate the inverse if it is not already available in the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
