

## makeCacheMatrix defines a 'special matrix' object (list) which contains
## functions that manipulate the data (oringal matrix and inverse) contained 
## within the 'object'. This exercise follows the 'mean' example defined in 
## the assignment write-up. 
##
## cacheSolve defines a function that operates on an object created with 
## makeCacheMatrix. It will calculate the matrix inverse on the first call to it
## store the result and return the inverse. Upon subsequent calls, it returns the 
## cached value of the matrix inverse. 
##


## makeCacheMatrix creates a 'special' matrix object which is actually
## a list of functions to manipulate the data within the object (in this case,
## the matrix 'inverse' value). 

makeCacheMatrix <- function(x = matrix()) {
      ## follow the analog of the 'mean' example 
      mi <- NULL #initializes the 'matrix inverse' to NULL
      set <- function(y) { ## defines a function to set the initial matrix 
                           ## and matrix inverses
            mo <<- y   ## save the orignal matrix object
            mi <<- NULL ## set the 'cache' inverse to null
      }
      get <- function() mo  ## return the 'matrix inverse' object mi
      setinverse <- function(im) mi <<- im  ## set the 'inverse' with parameter 'im'
      getinverse <- function() mi  ## return the current matrix inverse object 
      ## return the 'CacheMatrix' object ... list of functions 
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve defines a function which takes a 'special matrix' object created 
## with makeCacheMatrix and returns the inverse of the matrix (data) contained
## with the object. If the object already has the inverse calculated, that value
## is returned. Otherwise, the inverse is calculated, saved (cached), and returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      mi <- x$getinverse()  ## get the current inverse object 
      if(!is.null(mi)){  ## if the inverse has already been calculated, return that object
            message("getting cached matrix inverse")
            return(mi)  ## return the previously cached value of inverse
      }
      ##else 
      mo <- x$get()  ## get the original matrix 'data'
      mi <- solve(mo) ## use the solve function to calculate the inverse
      x$setinverse(mi) ## save the calculated inverse in 'cache' 
      mi  ## return the calculated inverse matrix
}
