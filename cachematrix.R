## Functions to create a holder object that contains
## a matrix and its inversion. The inversion matrix gets calculated
## on first use and gets cached.

## Create a special matrix with a inversion matrix cache.
## A cached inversion matrix will be dropped if you set a new
## underlying matrix.
makeCacheMatrix <- function(originalMatrix = matrix()) {
    cache <- NULL
    set <- function(newMatrix) {
        if (!identical(newMatrix,originalMatrix)) {
            message("resetting cache")
            originalMatrix <<- newMatrix
            cache <<- NULL
        }
    }
    get <- function() originalMatrix
    setInversion <- function(inversion) cache <<- inversion
    getInversion <- function() cache
    list(
        set = set, 
        get=get, 
        setInversion=setInversion, 
        getInversion=getInversion)
}


## Inverses (solves) a matrix. Use makeCacheMatrix to create a cacheable matrix
## for the input. The result will only be calculated once as long as the 
## underlying matri doesn't change.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversion <- x$getInversion()
    if(is.null(inversion)) {
        message("calculating inversion of matrix")
        data <- x$get()
        inversion <- solve(data, ...)
        x$setInversion(inversion)
    } else {
        message("getting cached inversed matrix")
    }
    inversion
}
