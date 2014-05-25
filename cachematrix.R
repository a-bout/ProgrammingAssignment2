## The first function, `makeCacheMatrix ` creates a special "matrix",
## which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse
## The second function calculates the inverse of the special "matrix"
## created with the above function. If inverse was calculated previously 
## it just returns value of inverse from cache, otherwise it
## calculate inverse and set the value of inverse in cache via
## the 'setinverse' function

## This function creates "matrix" object for incapsulating get, set and
## inverse obtaining logic

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(mat) {
		x <<- mat
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv 
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function returns cache of onverse if it exists, or calculate it.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
      if (!is.null(inv)) {
	      message("getting cached data")
            return(inv)
	}
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setinverse(inv)
      inv
}
