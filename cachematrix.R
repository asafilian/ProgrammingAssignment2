## Aliakbar Safilian

## There are two functions in this file: makeCacheMatrix & cacheSolve

## The makeCacheMatrix function creates a special "matrix", 
## which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

## The cacheSolve function calculates the inverse of a given "matrix" 
## created with the makeCacheMatrix function. 
## It first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets its value in the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){x}
        setInv <- function(MInv){ inv <<- MInv}
        getInv <- function(){inv} 
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of a given "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
