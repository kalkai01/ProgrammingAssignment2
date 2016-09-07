## These functions cache the inverse of a matrix, 
## if the inverse has already been calculated the value is pulled from cache
## otherwise it is calculated


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ##create variable 'inverse' and give value NULL
        inverse <- NULL
        
        ## set the value of the matrix 
        set <- function(y) {
                x <<- y
                invserse <<- NULL
        }
        
        ## get the value of the matrix 
        get <- function() x
        
        ## set the value of the inverse 
        setinverse <- function(inverse) inverse <<- solve
        
        ## get the value of the inverse 
        getinverse <- function() inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), cachesolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        ## if the value of 'inverse' is not NULL print cache message and print inverse
        if(!is.null(inverse)) {
                message("getting cached data")
                inverse
        }
       
        ## otherwise calculate inverse and print
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        
}
