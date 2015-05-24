## The functions allows for calculating the inverse of
## a matrix. The function caches the value when it is 
## first calculated, allowing for faster calculation the
## next time by simply reading the value.

## The function can be used to set and get 
## the matrix as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse){
	       I <<- inverse
	  }
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The function can be used to calculate the
## inverse of a matrix. It expects a matrix object
## created by the above function. The function assumes
## that the provided is invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data)
        x$setInverse(I)
        I
}
