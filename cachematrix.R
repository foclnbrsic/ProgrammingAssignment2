## Put comments here that give an overall description of what your
## functions do matrix solving to calculate the inverse of a matrix. When there
## is a cache, the cached value is returned. When not, a new value is set.

## Write a short comment describing this function
## function creates and returns four functions (get, set, getinv and setinv) to
## be called in the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inversed) inv <<- inversed
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function calculates the inverse of a matrix using solve function when no
## cache value is not found. When found, the function simply returns the cached 
## value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
