## The following functions creates and manipulates an type of matrix that is
## capable of chaching its inverse matrix

## This function creates an matrix object that caches its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(inverse) inv <<- inverse
    getsolve <- function() inv
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function caches or get the cached inverse matrix of an makeCacheMatrix
## object

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
