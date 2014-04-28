## makeCacheMatrix creates a special matrix object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL  
        }
        get <- function() x
        setinv <- function(solve) m <<- solve ##set 
        getinv <- function() m
        ## return a list of functions for the matrix object:
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## cacheSolve computes the inverse of the matrix created by makeCacheMatrix:

cacheSolve <- function(x, ...) {
        ## get inverse of matrix 'x' and store as m:
        m <- x$getinv()
        
        ## OR return the inverse if it has already been cached:
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        ## Return a matrix that is the inverse of 'x':
        m
}
