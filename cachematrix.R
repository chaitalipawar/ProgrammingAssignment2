## Assignment: Caching the Inverse of a Matrix
## Following are a pair of functions that can cache inverse of a matrix

## This function creates a 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y){
                x <<- y
                cache <<- NULL}
        get<-function() x
        setInverse<- function(newValue){
                cache <<- newValue
                }
        getInverse <- function() cache
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## This function checks whether the cached value of inverse of the matrix exists
## If the value exists, it returns the cached inverse else it computes the inverse of cached value of the matrix

cacheSolve <- function(x, ...) {
        cache <- x$getInverse()
        if(!is.null(cache)){
                message("inverse value exists")
                return(cache)
                }
        data <- x$get()
        cache <- solve(data,...)
        x$setInverse(cache)
        cache
        ## Return a matrix that is the inverse of 'x'
}
