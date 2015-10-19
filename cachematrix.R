## Put comments here that give an overall description of what your
## functions do

## This makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse. The argument x is
## assumed to be an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinverse <- function(invx) inv_x <<- invx
        getinverse <- function() inv_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## try getting cached inverse of x 
        xinv <- x$getinverse()
        
        ## if inverse of x is not NULL (i.e. cached), 
        ## return the cached inverse of x
        if(!is.null(xinv)) {
                message("getting cached inverse matrix...")
                return(xinv)
        }
        
        ## call solve on x and cache the resulting inverse 
        ## matrix by calling setinverse() from the makeCacheMatrix,
        ## then return inverse matrix
        xinv <- solve(x$get(), ...)
        x$setinverse(xinv)
        xinv
}
