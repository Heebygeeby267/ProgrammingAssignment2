##R Programming - Data Science Specialization
##Programming Assingment 2
##Heebygeeby267

##Last Edited: 7/6/2019

setwd("ProgrammingAssignment2")
getwd()

## Put comments here that give an overall description of what your
## functions do

## Matrix inversion is usually a costly computation and there may be benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## The first function, makeCacheMatrix creates a special "matrix" object that 
## can cache the inverse. The second function, cacheSolve, computs the inverse
## of the special "matrix" returned by the first function. If the inverse has already
## been calculated, then the cacheSolve will retrieve the inverse from the cache.


## Write a short comment describing this function

## Create a special "matrix" object that can cache its inverse"

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

## computes the inverse of a special matrix returned by makeCacheMatrix using the solve function. 
## If the inverse has already been calculated, return the value of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
