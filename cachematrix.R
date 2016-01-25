## These two functions create a matrix object to cache an inverse
## of a matrix since it is costly to compute over and over


## Creates a matrix object to cache inverse
## matrices since they are costly to compute.
## Assumes that X is a square invertible matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(inverse) inv <<- inverse
    getinvmatrix <- function() inv
    list( set = set, get = get,
    setinvmatrix = setinvmatrix,
    getinvmatrix = getinvmatrix)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    invmatrix <- x$getinvmatrix()
    
    ##Check if inverse has been calculated already
    if(!is.null(invmatrix)) {
        message("getting cached inverse matrix")
        return(invmatrix)
    }
    ##Calculate inverse matrix
    data <- x$get()
    invmatrix <- solve(data, ...)
    x$setinvmatrix(invmatrix)
    invmatrix
        
}
