## This file contains two functions,makeCacheMatrix() and cacheSolve() 
## The first function creates a matrix object that can cache the matrix's inverse 
## and the second function computes the inverse of the matrix, if it is not already computed else it returns the cached inverse

## This function creates a matrix object which returns a list containing
## i.   A function to set the matrix values
## ii.  A function to get the matrix
## iii. A function to set the inverse of the matrix
## iv.  A function to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	matrixInverse <- NULL
    		set <- function(y){
      			x<<-y
      			matrixInverse <<-NULL
    		}
    		get<- function() x
    		setInverse <- function(inv) matrixInverse <<- inv
    		getInverse <- function() matrixInverse
    	list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}


## It takes the matrix object returned by the function makeCacheMatrix and returns
## the cached value of the matrix's inverse if it is already calculated , else
## it calculates the inverse using the solve() function and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
	matrixInverse <- x$getInverse()
		if(!is.null(matrixInverse)){
			print("Getting the Cached inverse of the matrix")
			return(matrixInverse)
    		}
	mdata <- x$get()
	matrixInverse <- solve(mdata,...)
	x$setInverse(matrixInverse)
	matrixInverse

        ## Return a matrix that is the inverse of 'x'
}
