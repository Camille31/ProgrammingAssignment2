## pairs of functions that are used to create a special object that stores a matrix
## and caches its inverse


## create a special "matrix" able to cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        
        # set the value of the inverse
        setinverse <- function(inv) inverse <<- inv
        
        # get the value of the inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
