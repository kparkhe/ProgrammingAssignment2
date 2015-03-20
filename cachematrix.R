## makeCacheMatrix creates a cache with four functions: set the value of
## a matrix with set(y), and retrieve it with get(); set the value of the
## inverse with setinverse(inverse), and retrieve it with getinverse()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  
    ##Set the matrix, and make the value of its inverse NULL. Since matrix
    ##is changing, old value inv may have had is no longer valid.
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If the inverse of the matrix in x is already in the cache, simply return
## it. If not, add it to the cache and return it.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()      
    if(!is.null(inv)) { ##see if inverse is already in cache
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv) ##add inverse to cache
    inv  
}
