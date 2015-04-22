## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix 

## This function creates a special "matrix" object that can cache its inverse.
## The cache will be set to NULL everytime the object value is changed
## setInverse() should be used to set the matrix inverse
## getInverse() should be used to get the matrix inverse
## set() should be used to set the object value
## get() should be used to get the object value
## All above functions can be called using the returned list.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <- NULL
        }
        
        get <- function() {x}
        
        setInverse <- function(inv) {inverse <<- inv} 
        
        getInverse <- function() {inverse}
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        inverse
}
