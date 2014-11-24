## makeCacheMatrix() creates a "vector" that returns a list containing a global vector 
## variable and functions that operate on it to get, set, and calculate its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() returns the inverse of the given matrix, 
## retrieving the cached value if it exists
## or calculating the inverse if it doesn't

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## d9c1a3523c60b59df9979f331366a1059d4d2f9b