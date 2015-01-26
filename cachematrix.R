## This function creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        cachedVal <- NULL
        ## compare new matrix y versus x
        set <- function(y) {
                x <<- y
                cachedVal <<- NULL
        }
        get <- function() x
        setinv <- function(inv) cachedVal <<- inv
        getinv <- function() cachedVal
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        cachedVal <- x$getinv()
        if(!is.null(cachedVal)) {
                message("getting cached data")
                return(cachedVal)
        }
        data <- x$get()
        cachedVal <- solve(data, ...)
        x$setinv(i)
        cachedVal
}
