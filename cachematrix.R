## Put comments here that give an overall description of what your
## functions do

## The following function is used to create a special "matrix" object. That 
##matrix keeps the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {

        inverse_matrix <- NULL
        set <- function(y) {  #sets the matrix x with a given matrix y 
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x   #gets the defined matrix x 
        setinverse <- function(solve) inverse_matrix <<- solve
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the matrix x. First it checks
##whether the inverse is already calculated. If so, the function cacheSolve retreives
##it from the cache. If not, it performs calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
