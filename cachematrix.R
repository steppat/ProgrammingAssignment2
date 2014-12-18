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
##
##
## a function wich recieves a matrix and return a list
## with followin functions: 
## get() - returns the original matrix
## set(m) - set the matrix, inverse matrix to NULL
## setinverse(m) - sets the inversed matrix
## getinverse() - return the inversed matrix or NULL
makeCacheMatrix <- function(x = matrix()) {
        
        inverseMatrix <- NULL
        
        set <- function(pMatrix) {
          x <<- pMatrix
          inverseMatrix <<- NULL
        }
        
        get <- function() {
          x
        }
        setinverse <- function(pMatrix) inverseMatrix <<- pMatrix
        getinverse <- function() inverseMatrix
        
        list(get = get, 
             set = set,
             setinverse = setinverse, 
             getinverse = getinverse)
}

## function which recieves a matrix and calculates its inversed matrix,
## the inversed matrix gets cached
## cache is only used if x has not changed
cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
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
