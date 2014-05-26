# Matrix inversion is usually a costly computation and there may be some benefit (e.g., 
# saving time in an R session) to caching the inverse of a matrix rather than computing
# it repeatedly. 

# The first function, makeCacheMatrix creates a list containing a function to
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMat <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getMat <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(setMat=setMat, getMat=getMat, setInverse=setInverse, getInverse=getInverse)
}

#
# The next function returns the inverse of the matrix created above. It first checks
# whether the inverse has already been computed. If yes, it gets the computed inverse
# and skips the computation in this iteration. If not, it computes the inverse and 
# sets the value in the cache using the setinverse function.
# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$getMat()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}

