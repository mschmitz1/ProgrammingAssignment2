## Put comments here that give an overall description of what your
## functions do
## For a great matrix it takes time to compute the inverse of it (with the 
## solve function). So this two functions allow to store the computed inverse 
## matrix in the object which was created with makeCacheMatrix.
## cacheSolve will return the cached inverse if it was computed; otherwise
## cacheSolve computes the inverse and stores it in the cache.
##


## makeCacheMatrix takes an matrix as input and generates an object which 
## contains the input matrix and a variable to store the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
## 'x' must be a matrix; otherwise the function returns NA
	## 
	if (!class(x) == "matrix") {
		print("Input object is not a matrix")
		return(NA)
	}
	m <- NULL  ## Cache for solve value
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve 	## sets cache
	getsolve <- function() m					## gets cached value
	list(set = set, get = get,
		 setsolve = setsolve,
		 getsolve = getsolve)
}


## makeCacheMatrix takes an matrix as input and generates an object which 
## contains the input matrix and a variable to store the inverse of it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	## 'x' must be created with the makeCacheMatrix function
	## 
	m <- x$getsolve()	## m is the cached data
	if(!is.null(m)) {		## m was computed with the solve function
			message("getting cached data")
			return(m)
	}
	data <- x$get()
	m <- solve(data, ...)	## otherwise the inverse matrix has to be computed
	x$setsolve(m)			## and set in the cache
	m
}
