## This program contains a pair of functions that cache the inverse of a matrix.
## One function takes advantage of scoping rules of the R language in order to
## preserve the state inside of an R object. This program assumes that the matrix
## supplied to compute its inverce is always a square invertible matrix




## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    ## Cache the matrix 'x'
    
    set <- function(y) {
        
        x <<- y
        
        i <<- NULL
        
    }
    
    
    get <- function() x
    

    ## Take advantage of the scoping rules of the R language and 
    ## preserve state inside of an R object. Cache the inverse of a matrix 'x'
    
    setinverse <- function(inverse) i <<- inverse
    

    getinverse <- function() i
    
    
    ## Create a list 
    
    list(set = set, get = get,          
         setinverse = setinverse, getinverse = getinverse)

}




## This function computes the inverse of the matrix, if the inverse of matrix
## has already been calculated and cached (and the matirx has not changed)
## Else then this function retrieve the inverse from cache

cacheSolve <- function(x, ...) {

    ## Check whether the inverse of the matrix 'x' has already been calculated and cached.
    ## If so, return the inverse of the matrix 'x' retrived from cache and skips the computation.
    
    i <- x$getinverse()
    
    if(!is.null(i)){
        
        message("getting cached data")
        
        return(i)
    }
    
    ## Compute the inverse of matrix 'x' for first time and initiate cache and return it
    
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    i
    
}
