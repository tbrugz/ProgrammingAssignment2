## These are a couple of functions that may be used to create a special
## "matrix" object that caches it's inverse to potentially save time in
## time-consuming computations


## The function 'makeCacheMatrix' creates a special matrix object that
## can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() { x }
	setinverse <- function(y) { inv <<- y }
	getinverse <- function() { inv }

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function 'cacheSolve' tests if the inverse of the supplied matrix is
## already calculated. If it is, the function returns the inverse. If not,
## the function computes the inverse, caches it and then return the inversed
## matrix
cacheSolve <- function(x, ...) {
	if(!is.null(x$getinverse())) {
		message("getting cached matrix")
		return(x$getinverse())
	}
	message("solving and caching matrix")
	x$setinverse( solve(x$get(), ...) )
	x$getinverse()
}
