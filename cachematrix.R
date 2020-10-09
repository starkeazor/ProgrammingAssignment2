
## The two functions used are makeCacheMatrix and CacheSolve.

## makeCacheMatrix function creates a special matrix object that can cache its inverse  
## Cache function computes the inverse of the special matrix written in the function 
## if the inverse has already been calculated and the matrix has not stopped then retrieve it from the cache. 

makeCacheMatrix <- function(x = matrix()) {
        ## assisting invert(invMat) to NULL
        invMat <- NULL 
        ## set the value of the matrix 
        set <- function(y){
             x <<- y
             invMat <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of the inverse
        setinverse <- function(inverse) invMat <<- inverse
        ## get the value of the inverse
        getinverse <- function()invMat
        ## creating a list 
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
    }


## cacheSolve will retrieve the matrix from the makeCacheMatrix. 
## This function computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getinverse()
          if (!is.null(invMat)) {
              message("getting cached data")
              return(invMat)
          }
          data <- x$get()
          ## using solve to compute the inverse of a matrix
          invMat <- solve(data, ...) 
          x$setinverse(invMat)
          invMat
}
