## Put comments here that give an overall description of what your
## functions do

# both functions strongly based on on the examples of the R-programmin assignment on Courseer
# (https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping):


## Write a short comment describing this function
#
# creates a list providing functions for matrix manipulation
# set -> creates the matrix
# get -> prints/returns the matrix
# setinv -> stores provided object in member inv of the matrix (should be the inverse of x)
# getinv -> returns object inv of the matrix (should be the inverse of x)
#
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#
# use functions of provided by makeCacheMatrix
# if some object is stored in inv, this object is returned
# otherwise the inverse is calculated and stored using
# makeCacheMatrix's and solve functions and
#
cachesolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
