## These functions creates a special matrix object that can cache its inverse and
## computes the inverse of the matrix criated

## The follow function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }                                         #set the matrix
        get <- function() x                       #get the matrix
        setinv <- function(inv) i <<- t(x)        #set the inverse of the matrix
        getinv <- function() i                    #get the inverse of the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)                     #get a list with previous functions
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }                                    ## View if the inverse of previous matrix has already been calculated
        data <- x$get()
        i <- solve(data, ...)   ## Return a matrix that is the inverse of 'x'
        x$setinv(i)
        i
}
