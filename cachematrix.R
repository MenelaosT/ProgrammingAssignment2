## Create two function one that caches the result of the matrix inversion 
## and another that actually performs the calculation. If the matrix-result is cached the calculation is skipped.

## Code that caches a matrix. Returns a list 
## that sets the matrix, gets the matrix, sets
## the result of the inverse of the matrix, 
## gets the result fo the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x<<-y
		m<<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set = set, 
             get = get,
	     setmatrix = setmatrix,
	     getmatrix = getmatrix)	
}


## Take input of a square matrix it calculates the inverse of this matrix.
## if the results exists/cached it picks up that results, else it performs
## the calculation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m<-x$getmatrix()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-x$get()
	m <- solve(matrix,...)
	x$setmatrix(m)
	m

}
