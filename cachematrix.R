## Put comments here that give an overall description of what your
## functions do
##
## Function makeCacheMatrix creates a list of four functions for 
## provided matrix. The member functions do following things.
##
## Function get :
## Input arguements : Nil
## Return value : It returns the current matrix, for which an inverse
##                matrix is already cached in a list returned from 
##                makeCacheMatrix
##
## Function set(input_matrix) :
## Input arguements : New matrix (input_matrix) to be inverted
## Return value : Nil. It is there to replace the current matrix by 
##                a new matrix (y). It also resets the cached inverse
##                to NULL 
##
## Function getinverse() :
## Input argument : Nil
## Return value : The current value of inverted matrix. If it is not 
##                yet calculated then, NULL
##
## Function setinverse(inv_mat)
## Input_arguement :Newly calculated inverse matrix (inverse_matrix) 
##                  to be cached.
## Return value : Nil. It is there to cache the last value of inverse
##                matrix
## Write a short comment describing this function
## makeCacheMatrix creates a list of 4 functions
makeCacheMatrix <- function(x = matrix()) {
	inverse_matrix <- NULL
#   return the current matrix
	get <- function()	x
#   set the new matrix and reset the inverse 
	set <- function(input_matrix) {
		x <<- input_matrix
		inverse_matrix <<- NULL
	}
#   get current (cached) inverse matrix
	getinverse <- function() inverse_matrix
#   cache the newly calculated inverse
	setinverse <- function(inv_mat) inverse_matrix <<- inv_mat	
#   Return the list of above four functions
	list(get = get, set = set,
		getinverse = getinverse,
		setinverse = setinverse)
}
## Write a short comment describing this function
## cacheSolve, checks if there is a cached inverse matrix of provifded
## matrix. If it is there it returns cached value. Else it calculates 
## the inverse and then caches the calculated inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## x should be the output of makeCacheMatrix(some_matrix)
	im <- x$getinverse()
	if(!is.null(im)) {
		message("Getting the cached inverse")
		return(im)	
	} else {
		message("Calculating the inverse")
		mat <- x$get()	
		im <-solve(mat)
		x$setinverse(im)
		im
	}
}

testruncache <- function() {
	xl <- makeCacheMatrix(matrix(c(1,2,3,4),byrow = TRUE, nrow = 2))
	for(i in 1:10) {
		cacheSolve(xl)
		print(xl$getinverse())
	}
	message("*********************************************************************************************")
	xl$set(matrix(c(5,6,7,8), byrow = TRUE , nrow = 2))
	for(i in 1:10) {
		cacheSolve(xl)	
		print(xl$getinverse())
	}
}
