## makeCacheMatrix() creates a "vector" that returns a list containing a global vector 
## variable and functions that operate on it to get, set, and calculate its inverse

makeCacheMatrix <- function(x = matrix()) {
xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() xinv
        setinv <- function(inv) xinv <<- inv
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() returns the inverse of the given matrix, 
## retrieving the cached value if it exists
## or calculating the inverse if it doesn't

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}
