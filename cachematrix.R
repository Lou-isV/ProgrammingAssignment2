## Put comments here that give an overall description of what your
## functions do

## These functions take an invertable matrix as input(x) and calculates its inverse
## then stores that result in a cache to be retrieved later if the matrix has not been
## changed. If the matrix has changed, it will clear the cache and calculate the inverse 
## of the new matrix and cache that result. The main goal of these functions is to save
## time when doing time consuming computations.

## Write a short comment describing this function

## makeCacheMatrix creates a list of functions that set the value of the matrix,
## get the value of the matrix, set the inverse of the matrix, and get the value
## of the matrix. This function will also clear the values if a new matrix is used
## as the input (x).

makeCacheMatrix <- function(x = matrix()) {
       invm = NULL 
       setmatr = function(y) {
              
              x <<- y 
              invm <<- NULL 
       }
       
       getmatr = function() x
       setinvmatr = function(inversem) invm <<- inversem
       getinvmatr = function() invm
       
       list(setmatr = setmatr, getmatr = getmatr, 
            setinvmatr = setinvmatr, getinvmatr = getinvmatr)

}


## Write a short comment describing this function

## cacheSolve calculate the inverse of the matrix that was created with makeCacheMatrix.
## Before calulating the inverse, cacheSolve will check to see if the result has already
## been cached. If the result is cached, it will not do a new calculation and display the message "getting cached data"
## the message "getting cached data" and display the cached result to save time.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       invm <- x$getinvmatr() 
       
       if(!is.null(invm)){ 
              
              message("getting cached data")
              return(invm)
       }
       
       datm <- x$getmatr()
       invm <- solve(datm, ...)
       
       x$setinvmatr(invm)
       
       return(invm)
}

## Information from Stack Overflow and masterr.org helped guide me with this assignment and also
## increased my understanding of what is happening in each line of code.
## http://stackoverflow.com/questions/25374803/returning-the-inverse-matrix-from-a-cached-object-in-r-checking-that-input-matri
## http://masterr.org/r/how-to-cache-a-matrix-inversion-in-r/
