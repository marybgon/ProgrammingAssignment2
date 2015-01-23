## There are two functions that will help a matrix inversion. Matrix inversion is a costly computation so caching its inversion avoid computing it repeatedly

## This function creates a list of functions that caches the matrix inversion. The function set, sets the value of the matrix. Using the function get we can get the value of the matrix. Function setsolve sets the inversion of the matrix which we can get by the getsolve function

makeCacheMatrix <- function(x = matrix()) {
	
	m<- NULL
	set <- function (y){
		x <<- y
		m <<- NULL
	}
	get <- function () x
	setsolve <- function (solve) m<<- solve
	getsolve <- function () m
	list(set = set, get = get, setsolve=setsolve, getsolve=getsolve)
	
}


## This function check if the inversion matrix is already calculated. If so, it displays the message ("getting cached data") and return the inverted matrix. If not, it calculates the matrix inversion and sets it using setsolve function

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
