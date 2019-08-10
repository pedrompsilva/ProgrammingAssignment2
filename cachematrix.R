## Put comments here that give an overall description of what your
## functions do

## this function returs a list object containing a set of functions  able 
## to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ##variable to cache the matrix inverse
    iCache <- NULL
    m <- x
    
    ## sets the matrix and resets any existing cache
    setMatrix <- function (y) {
        iCache <<- NULL
        m <<- y      
    }
    
    ## returns the original matrix
    getMatrix <- function () m
    
    ## caches the inverse matrix
    setInverse <- function (inv) iCache <<- inv
    
    ## returns the cached matrix inverse 
    getInverse <- function () iCache 
    
    ##return "special matrix" object as a list of functions  
    list(setMatrix=setMatrix, getMatrix=getMatrix,getInverse=getInverse, setInverse=setInverse)
  
}


## the cacheSolve function returns the inverse of a matrix
## created with the function makeCacheMatrix() 
## in case the inverse matrix was calculated before it will return the cached matrix
## otherwise it will calculate and cache it

cacheSolve <- function(x, ...) {
    
    ## get the current cached inverse matrix
    i <- x$getInverse()
    
    
    ##eval existing cache 
    if (is.null(i)){
        #cache is empty, then calc inverse and cache it
        
          i<-solve(x$getMatrix())
      
          x$setInverse(i)
        
          return(i)
    }
    else{
      
          ## cache is not empty, return cached inverse
          message("getting cached inverse matrix")
        
          return(i)
    }
}