## The makeCacheMatrix(..) is a function which receives a matrix and returns a list 
## object with some internal get and set functions. get/set functions are to return 
## and receive the matrix, getinverse/setinverse are to return and receive the 
## matrix inverse.  
##
## The cacheSolve(..) is a function which computes the matrix inverse and caches 
## the result.
##
## Usage:
## source("cachematrix.R")
##
## amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## matrix$get()
##     [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##
##amatrix$getinverse()
##NULL
##
##cacheSolve(amatrix)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##amatrix$getinverse()
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##cacheSolve(amatrix)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
##
##
## A function wich receives a matrix and returns a list
## with following functions: 
## get() - returns the original matrix
## set(m) - set the matrix, matrixInverse to NULL
## setinverse(m) - set the matrixInverse
## getinverse() - returns the matrixInverse or NULL
makeCacheMatrix <- function(x = matrix()) {
        
        matrixInverse <- NULL
        
        set <- function(pMatrix) {
          x <<- pMatrix
          matrixInverse <<- NULL
        }
        
        get <- function() {
          x
        }
        setinverse <- function(pMatrix) matrixInverse <<- pMatrix
        getinverse <- function() matrixInverse
        
        list(get = get, 
             set = set,
             setinverse = setinverse, 
             getinverse = getinverse)
}

## Function which receives the special object created by 'makeCacheMatrix' and 
## calculates its matrix inverse if it was not calculated before.
## The matrix inverse gets cached by calling the 'setinverse' function.
## To invert the matrix the solve function was used.
cacheSolve <- function(x, ...) {
  
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
  
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setinverse(inverse)
        inverse
}
