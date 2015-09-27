## Matrix inversion is usually a costly computation and there may be some 
## 	benefit to caching the inverse of a matrix rather than compute it 
## 	repeatedly.  This function allows for the cachein the Inverse of a 
##	Matrix


## makeCacheMatrix: This function creates a special "matrix" object that can 
## 	cache its inverse.  The first function, makeMatrix creates a special 
##	"matrix", which is really a list containing a function to:
##		1. set_matrix: set the values of the matrix
##		2. get_matrix: get the values of the matrix
##		3. set_inverse_matrix: set the values of the inversed matrix
##		4. get_inverse_matrix: get the values of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
	cached_matrix <- NULL
	set_matrix <- function(y) {
		x <<- y
		cached_matrix <<- NULL
	}
	get_matrix <- function() x 
		message("caching matrix")
	
	set_inverse_matrix <- function() {
		cached_matrix <<- solve(x)
		#message("returning indexed matrix")
	}	
	get_inverse_matrix <- function() {
		if(is.null(cached_matrix)) set_inverse_matrix()
		cached_matrix
		
	}
	list(set_matrix = set_matrix,
		get_matrix = get_matrix,
		set_inverse_matrix = set_inverse_matrix,
		get_inverse_matrix = get_inverse_matrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## 	returned by makeCacheMatrix above. If the inverse has already been 
## 	calculated (and the matrix has not changed), then the cacheSolve will 
## 	retrieve the inverse from the cache.  If the inverse has not been 
##	it will cache the matrix and return the inversed matrix

cacheSolve <- function(x, ...) {
        
	#Check to see if x has been chached..
	if(is.recursive(x)){
	  message("matrix is cached")
		cached_matrix <- x$get_inverse_matrix()
		message("returning previously indexed matrix")
		return(cached_matrix)
	}
	message("matrix is not cached")
	#If not, cache matrix
	not_cached_matrix <- makeCacheMatrix(x)
	cached_matrix <- not_cached_matrix$get_inverse_matrix()
	message("returning indexed matrix")
	cached_matrix
	
}