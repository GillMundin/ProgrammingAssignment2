## makeCacheMatrix creates a test matrix, sets the invmat variable to NULL on creation
## or when the test matrix changes, and creates a list of variable names that can be 
## called in cacheSolve

## cacheSolve looks to see if the inverse has already been calculated, retrieves the 
## cached value if it has, and calculates and returns the inverse if it hasn't, or if
## the test matrix has changed since the previous cache

## A function to create a test matrix, in order to find the inverse of the test matrix

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y){
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invmat <<- inv
        getinv <- function() invmat
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## A function to return the inverse of the test matrix, if already cached, 
## or to compute the inverse initially, or when the test matrix changes

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinv()
        if(!is.null(invmat)){
                message("Getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinv(invmat)
        invmat
}
