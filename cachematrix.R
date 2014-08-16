## This is the first function that creates a matrix object for inverse. 
## This matrix contains a list containing functions for setting and getting matrix
## The functions also set and get values of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        xi <- NULL
        set <- function(y) {
                x <<- y
                xi <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) xi <<- inverse
        getinverse <- function() xi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This second function computes the matrix inverse from first function. 
## For already created inverses, this function will retrieve inverse from cache, 
## else recalculate inverse and then cache

cacheSolve <- function(x, ...) {
        xi <- x$getinverse()
        if(!is.null(xi)) {
                message("getting cache data")
                return(xi)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}