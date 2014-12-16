## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates an object 
## containing a matrix and its inverse
## and 4 functions to set/get the matrix and its
## inverse, the inverse matrix is cached.
##

makeCacheMatrix <- function(x = matrix()) {
	matinv <- NULL
	set <- function(y) {
	   x <<- y
	   matinv <<- NULL
	}
	get <- function() x
	setinv <- function(matrix_inv) matinv <<- matrix_inv
	getinv <- function() matinv
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)	
}


## Write a short comment describing this function
## cacheSolve is a function
## that calculates or takes the cached value
## of the inverse matrix of a makeCacheMatrix
## depending on whether the inverse has already
## been calculated
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	matinv <- x$getinv()
	if (!is.null(matinv)) {
	   message("getting cached data")
	   return(matinv)
	}
	data <- x$get()
	matinv <- solve(data, ...)
	x$setinv(matinv)
	matinv	   
	
}
