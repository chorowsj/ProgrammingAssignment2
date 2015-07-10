## Creates a "matrix" object, being a list containing functions to
## [1] set the value of the matrix
## [2] get the value of the matrix
## [3] set the inverse of the matrix
## [4] get the inverse of the matrix (no error returned if the matrix is not invertible)
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function (y) {
			x <<- y
			i <<- NULL
		 }
	get <- function () x
	setInverse <- function (inverse) i <<- inverse
	getInverse <- function () i
	list (set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse) 
}

## Calculates the inverse of the matrix provided by makeCacheMatrix function
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
