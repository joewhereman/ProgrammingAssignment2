## These functions were written for the week 3 assignment of
## Coursera's Data Science: R Programming Course

makeCacheMatrix <- function(x = matrix()) {
## This function makes a matrix object that can cache its inverse within R.
## First, it defines the default format of the input.
## Then, it preps the inverse matrix container and
## defines a function which resets the given matrix if the old one is antiquated.
##Lastly, this function retrieves the matrix argument, assigns the inverse value,
## and sets it up for future reference.

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invm <<- inverse
    getinverse <- function() invm
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
 }
 
## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If this was already calculated, then cacheSolve will get the cahced inverse
 
 cacheSolve <- function(x, ...) {
    invm <- x$getinverse()
    if(!is.null(invm)) {
        message("finding cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinverse(invm)
    invm
 }
            
