## This function creates a special "matrix" object that can cache its inverse.
## 'x' is the argument of this function and assumed to be an invertible matrix
## 'm' is the variable which store the inverse of matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
        
        ## set the value of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the value of the matrix
        get <- function() x
        
        ## set the value of the inverse of the matrix
        setinv <- function(inv) m <<- inv
        
        ## get the value of the inverse of the matrix
        getinv <- function() m
        list(set = set, get = get,
             setinvn = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        
        ## check to see if the inverse has already been calculated
        ## if so get the inverse from the cache and skip
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## otherwise, calculates the inverse of the data
        ## and sets the value of the inverse in the cache via the 'setinv' function 
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
