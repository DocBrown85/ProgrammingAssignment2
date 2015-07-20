## Put comments here that give an overall description of what your
## functions do

###########################################################################
##			R Programming Assignment 2			 ##
###########################################################################
##									 ##
## makeCacheMatrix:	creates a "special" matrix object that can cache ##
##			its inverse					 ##
##									 ##
## cacheSolve:		computes the inverse of the special "matrix"	 ##
##			returned by makeCacheMatrix			 ## 
##									 ##
###########################################################################

## Write a short comment describing this function

###########################################################################
## FUNCTION: makeCacheMatrix 						 ##
###########################################################################
##									 ##
## This function creates a "special" matrix object that can cache its 	 ##
## inverse using lexical scoping of R Language. This can serve to avoid  ##
## time-consuming computation when not needed.				 ##
##									 ##
## INPUT								 ##
##	param1: a matrix object x, default value for x is the		 ##
##		1X1 empty matrix					 ##
##									 ##
## OUTPUT								 ##
##	the "special" matrix object that can cache its inverse matrix	 ##
##									 ##
###########################################################################

makeCacheMatrix <- function(x = matrix()) {

	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverseMtx) inverse <<- inverseMtx
	getInverse <- function() inverse
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)


}


## Write a short comment describing this function

###########################################################################
## FUNCTION: cacheSolve 						 ##
###########################################################################
##									 ##
## This function computes the inverse of the "special" matrix returned	 ##
## by makeCacheMatrix. If the inverse has already been calculated and	 ##
## the matrix has not changed, then the cacheSolve retrieves the inverse ##
## from the cache, otherwise computes the inverse matrix and caches its  ##
## value in the "special" matrix object.				 ##
##									 ##
## INPUT								 ##
##	param1: a "special" matrix object created with makeCacheMatrix	 ##
##	param2: additional var args options to pass to solve() when 	 ##
##	        computing inverse matrix				 ##
##									 ##
## OUTPUT								 ##
##	the inverse of the special "matrix" returned by			 ##
##	makeCacheMatrix							 ##
##									 ##
###########################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inverse <- x$getInverse()
	if (!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	matx <- x$get()
	inverse <- solve(matx, ...)
	x$setInverse(inverse)

	inverse
}
