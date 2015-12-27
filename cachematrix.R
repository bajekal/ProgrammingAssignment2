## Put comments here that give an overall description of what your
## functions do
#  
# Matrix inversion is a computationally expensive algorithm. For larger 
# matrices, there is a benefit to caching the computed inverse of a matrix 
# in cases where the data has not changed.

#
# This caching operation is conducted in two functions:
# a. makeCacheMatrix: creates a special "matrix" object that can 
#                     cache its inverse
# b. cacheSolve:      Checks the existance of a previously computed and 
#                     cached inverse. If the inverse does not exist, it 
#                     computes the inverse of a matrix and caches the result.

# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
#
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse
#
makeCacheMatrix <- function(x = numeric()) {

		m <- NULL
		set <- function(y) {
		    x <<- y
		    m <<- NULL
		}
		get <- function() x
		setsolve <- function(solve) m <<- solve
		getsolve <- function() m
		list(set = set, get = get, 
		     setsolve = setsolve,
		     getsolve = getsolve)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setsolve`
# function.

cacheSolve <- function(x, ...) {


        ## Return a matrix that is the inverse of 'x'

	m <- x$getsolve()
	if(!is.null(m)) {
	    message("getting cached data")
	    return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
