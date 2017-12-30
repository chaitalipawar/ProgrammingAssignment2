## Assignment: Caching the Inverse of a Matrix
## Following are a pair of functions that can cache inverse of a matrix

## This function creates a 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        ## Create a variable to store the inverse
        cache <- NULL
        
        ## Create a function to store the matrix in variable x
        set <- function(y){
                x <<- y
                cache <<- NULL
                }
        
        ## Create a function to retrieve matrix stored in x
        get<-function() x
        
        ## Create a function to set inverse of the matrix
        setInverse<- function(newValue){
                cache <<- newValue
                }
        
        ## Create a function to retrieve inverse of the matrix
        getInverse <- function() cache
        
        ## Create list of the above functions
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## This function checks whether the cached value of inverse of the matrix exists
## If the value exists, it returns the cached inverse else it computes the inverse of cached value of the matrix

cacheSolve <- function(x, ...) {
        
        ## Retrive inverse of a matrix
        cache <- x$getInverse()
        
        ## Retrieve inverse from the cache and display message if the value is not null
        if(!is.null(cache)){
                message("cached inverse value exists")
                return(cache)
                }
        
        ## Get the matrix created by getCacheMatrix and store the value in "data"
        data <- x$get()
        
        ## Calculate the inverse of the matrix
        cache <- solve(data,...)
        
        ## Store inverse in the cache
        x$setInverse(cache)
        
        ## Return inverse
        cache
}
