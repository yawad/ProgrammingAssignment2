## This is a set of functions to calculate the inverse of a matrix using solve()
## and caching the result in case the inverse is required for the same matrix.  

## This function saves the matrix and caches the result of the inverse computation
## makeCacheMatrix$set(x): sets the matrix
## makeCacheMatrix$get(): gets the matrix
## makeCacheMatrix$setinverse(x): saves the inverse computation in cache
## makeCacheMatrix$getinverse(): computes the inverse of a matrix
makeCacheMatrix <- function( x = matrix() ) {
 
    inv<-NULL
    
    set<-function(y) {
        x <<- y
        inv <<- NULL
    }
    get<-function() x
    setinverse<-function(inverse) inv <<- inverse
    getinverse<-function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of a matrix, from cache if it exists, or otherwise by computation
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
