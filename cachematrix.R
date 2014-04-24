## Programming Assignment 2
## Set of functions to calculate the inverse of a matrix, and cache the results to avoid repeating computation.

## makeCacheMatrix creates a list of functions to set and get a matrix, and to set and get the inverse of said matrix.
## Input:   matrix() object.
## Output:  list() object with 4 functions (set, get, setinv, getinv)
##          The first 2 set and get the matrix, and the other 2 set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve calculates the inverse of a matrix. It caches the results to avoid repeating computation.
## Input:   a "Matrix" object (the output of makeCacheMatrix) and an optional list of arguments to be passed to the solve() function.
## Output:  matrix() object (the inverse)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
   	inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
