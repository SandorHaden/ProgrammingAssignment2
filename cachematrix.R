
# Computing the inverse of a square matrix can be done with the solve function.
# For example, if X is a square invertible matrix, then solve(X) returns its
# inverse.
# Matrix inversion is usually a time consuming computation and there may be
# some benefit to caching the inverse of a matrix rather than compute it
# repeatedly.
#
# Below is a pair of functions that are used to create a special object
# that stores a matrix and cache's its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                # <<- operator can be used to assign a value to an object
                # in an environment that is different from the current
                # environment.
                x <<- y
                inv <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        getinverse <- function() {
                inv
        }

        # The makeCacheMatrix function creates a list containing functions to
        #    1.) set the value of the matrix
        #    2.) get the value of the matrix
        #    3.) set the value of the inverse matrix
        #    4.) get the value of the inverse matrix

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# The cacheSolve function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {

        inv <- x$getinverse()

        # It first checks to see if the inverse has already been calculated
        # (and the matrix has not changed).

        # If so ...
        if(!is.null(inv)) {

               # it gets the inverse from the cache
               message("getting cached data")

               # and 'return' exits the function (skips the resting computation)
               return(inv)
        }

        # Otherwise, it calculates the inverse of the matrix and sets the value
        # in the cache via the setinverse function.

        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
