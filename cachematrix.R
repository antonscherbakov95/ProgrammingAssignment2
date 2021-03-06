

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

         inv <- NULL ##NULL,since will hold matrix value
        set <- function(y) { 
                x <<- y  ##Value of Matrix
                inv <<- NULL ##If new reset to NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv ##gets inv when called
        list(set = set, get = get,
             setinv = setinv, 
             getgetinv = getinv)
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
