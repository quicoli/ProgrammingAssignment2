## This is the solution for Programming Assignment 2, from R Programming Course
##
## Overall
## =======
## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## cacheSolve: computes the inverse of the special "matrix" returned by 
##             makeCacheMatrix. If the inverse has already been calculated 
##             (and the matrix has not changed), then cacheSolve should 
##             retrieve the inverse from the cache.


## makeCacheMatrix
## ---------------
## x: a square invertible matrix
## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverted = NULL
        set = function(y) {
                # '<<-' is a operator for assigning a value to an object 
                # in an environment different from the current environment. 
                x <<- y  
                inverted <<- NULL
        }
        get = function() x
        setinv = function(inverse) inverted <<- inverse 
        getinv = function() inverted
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve
## ---------------
## x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
        inverted = x$getinv()
        
        # if inverse has already been calculated
        # I get from cache
        if (!is.null(inverted)){
                message("getting data from cache")
                return(inverted)
        }
        
        # else, calculates the inverse 
        mat.data = x$get()
        inverted = solve(mat.data, ...)
        
        # setting the value of the inverse in the cache
        x$setinv(inverted)
        
        return(inverted)
}