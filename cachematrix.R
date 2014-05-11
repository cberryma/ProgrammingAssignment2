## Overall caching the inverse of a matrix through
## these fuctions. First function creates a special 
## matrix that stores the matrix and caches the 
## inverse matrix

## This function creates a matrix object

makeCacheMatrix <- function(x = matrix(y:y)){
    i <- NULL
    set <- function(z){
        x <<- z
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function computes the inverse of
## the matrix created by the function above
cacheSolve <- function(x, ...){
	## Return a matrix that is the inverse of 'x'
    i <-x$getinv()
    if(!is.null(x)){
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(x, ...)
    x$setinv(i)
    i
}  