## These functions calculate the inverse of an (assumed) invertible matrix
## and store the inverse in cache, retreiving it as required (rather than
## recalculating it)

## Storing functions in a list used by the caching function

makeCacheMatrix <- function(mat = matrix()) {
	matinv <- NULL
	set <- function(y) {
		mat <<- y
		matinv <<- NULL
	}
	get <- function() mat
	setinv <- function(solve) matinv <<- solve
	getinv <- function() matinv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## Check cache for stored matrix inverse. If available, retrive cached
## matrix inverse, otherwise calculate matrix inverse

cacheSolve <- function(mat, ...) {
	## Return a matrix that is the inverse of 'x'
	matinv <- mat$getinv()
	if(!is.null(matinv)) {
		message("getting cached data")
		return(matinv)
	}
	data <- mat$get()
	matinv <- solve(data, ...)
	mat$setinv(matinv)
	matinv
}
