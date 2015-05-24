## Wraps a matrix in a way that its inverse can be cached
## This contravenes the basic nature of abstraction that is fundamental to OOP,
## but is appropriate for the assignment

## Makes a matrix wrapper with functions get, set, getsolution, setsolution
## get and set allow the values to be retrieved or changed
## getsolution will retrieve the cached value
## setsolution will set the cached value
## Note: Calling set will invalidate the cached value

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolution <- function(solution) s <<- solution
    getsolution <- function() s
    list(set = set, get = get,
         setsolution = setsolution,
         getsolution = getsolution)
}


## Given a "cache matrix", returns the cached solution if available, otherwise
## calculates, caches and then returns the result

cacheSolve <- function(x, ...) {
    s <- x$getsolution()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolution(s)
    s
}
