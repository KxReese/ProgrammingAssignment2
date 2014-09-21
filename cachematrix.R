## Given an invertible matrix, 'makeCacheMatrix' and 'cacheSolve'
## calculate and then cache the inverse matrix, allowing the inverse to
## be retrieved from cache in future instances instead of recalculated.  

## 'makeCacheMatrix' creates a list of functions to do the following: set 
## the value matrix, get the value matrix, set the inverse matrix and get
## the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
	## Initializes 'i' with NULL and sets the value matrix
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	## Uses solve to set the inverse of the value matrix
	setinv <- function(solve) i <<- solve
	getinv <- function() i
	## Returns the list of the set, get, setinv and getinv functions
	list(set = set, get = get, 
		setinv = setinv,
		getinv = getinv)
}


## 'cacheSolve' first checks to see if the inverse of the matrix has 
## already been calculated and cached. It prints the cached inverse matrix
## or calculates the inverse matrix if one has not already been cached.  

cacheSolve <- function(x, ...) {
		## Checks to see if the inverse has already been stored
		i <- x$getinv()
		if(!is.null(i)) {
			message("getting cached data")
			return(i)
		}
		## If inverse has not been stored, the inverse is calculated and cached
		data <- x$get()
		i <- solve(data, ...)
		x$setinv(i)
        ## Returns a matrix that is the inverse of the given matrix 'x'
		i
}
