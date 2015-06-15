## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. These pair of functions below can cache and compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	set <- function(y){
		matrix <<- y
		inv <<- NULL
	}
	
	get <- function(){
        return(matrix)
    }
	
	getInverse <- function() {
		return(inv)
	}
	
	return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## This function computes the inverse of the special "matrix" returned by "makeCacheMatrix" function. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
	
	if(!is.null(inv)) {
		message("Getting cached data..")
		return(inv)
	}
	
	data <- x$get()
	inv <- solve(data %*% data
	x$setInverse(inv)
	inv
}
