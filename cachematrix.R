## Put comments here that give an overall description of what your
## functions do
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.

# The following two functions are used to cache the inverse of a matrix.

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
# 1. sets + gets the value of the matrix
# 2. sets + gets the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation as in example. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Updated the Code for this assignment with R Studio and copied here. Not sure how to run the code here in GitHub.
## Here is the input and output to the functions
#> x = rbind(c(1, 1/10), c(1/10, 1))
#> m = makeCacheMatrix(x)
#> #m$get()
  #   [,1] [,2]
#[1,]  1.0  0.1
#[2,]  0.1  1.0
#> cacheSolve(m)
#           [,1]       [,2]
#[1,]  1.0101010 -0.1010101
#[2,] -0.1010101  1.0101010
#> 
