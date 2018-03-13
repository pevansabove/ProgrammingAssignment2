## This is programming assignment 2.

## Make cache matrix.
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

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    theinverse <- x$getinverse()
    if(!is.null(theinverse)) {
        message("getting cached data")
        return(theinverse)
    }
    thematrix <- x$get()
    theinverse <- solve(thematrix, ...)
    x$setinverse(theinverse)
    theinverse
}