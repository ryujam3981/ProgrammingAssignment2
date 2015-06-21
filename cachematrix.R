## These 2 functions work together to calculate and store the inverse of a 
## invertible matrix.

## function makeCacheMatrix:
## supplied with a invertible matrix, saves it.
## The actual calculation and the act of storing the inverse is done from cacheSolve.
## However, the value is stored in this function(environment)
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## function set - change the matrix x in makeCacheMatrix
    set <- function(y) {
        ##substitute the matrix x in the main function with the input y 
        x <<- y
        ## reinitializes inverse in makeCacheMatrix
        inv <<- NULL
    }
    ## function get - returns the matrix x stored in makeCacheMatrix
    get <- function() x
    ## function setinverse - store the value of the input in a variable inv 
    ## in the main function
    setinverse <- function(solve) inv <<- solve
    ## function getinverse - returns the stored variable inv
    getinverse <- function() inv
    
    ## store the functions
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## function cacheSolve:
## Takes the makeCacheMatrix object supplied to it, checks to see if inverse is stored in it.
## else, calculates the inverse and saves it back in the supplied makeCacheMatrix object
cacheSolve <- function(x, ...) {

    ## retrieve the matrix from the main function
    inv <- x$getinverse()
    ## check if inverse is stored in the main function,
    ## if found, return the saved inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if not stored, get the matrix, calculate inverse of the matrix, 
    ## save the inverse, return the value.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv 
}
