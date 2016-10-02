## We have two functions here: 
## makeCacheMatrix() : Creates a matrix object that caches the inverse of the 
## matrix itself

## cacheSolve(): This function is used to obtain the inverse of the matrix 
## object that is created in the makeCacheMatrix() function. The purpose of 
## creating a cache here is that if the inverse of the matrix has already been, 
## this function retrieves it from the cache, else the function computes the 
## inverse of the matrix 

##---------------------------------------------------------------------------##

## The function takes a square-invertibe matrix as input and returns a list
## consisting of functions that do the following: a) Sets the matrix
## b) Gets the matrix
## c) sets the inverse of the matrix
## d) gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    matInv <- NULL
    set <- function(y) {
        x <<- y
        matInv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(matrixInverse) matInv <<- matrixInverse
    getinverse <- function() matInv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The cacheSolve() function takes the output of the makeCacheMatrix() function
## as the input, and then returns the inverse of the matrix fed into the 
## makeCacheMatrix function()

cacheSolve <- function(x, ...) {
        
    matInv <- x$getinverse()
    if(!is.null(matInv)) {
        message("getting cached data")
        return(matInv)
    }
    matrixData <- x$get()
    matInv <- solve(matrixData, ...)
    x$setinverse(matInv)
    matInv
    
}
