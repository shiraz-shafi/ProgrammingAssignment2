## Put comments here that give an overall description of what your
## functions do

## A pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function

## This function below creates a special "matrix" object where the inverse will be cached.


makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

## This function calculates the inverse of the special "matrix" returned by makeCacheMatrix
## The cacheSolve will retrieve the inverse from the cache if the inverse has already been 
## computed and the matrix unchanged

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
