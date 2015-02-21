# This module defines a special matrix that caches both the matrix and 
# the result of the inverse matrix calculation. 
#
# Matrix inversion in most cases is a costly computation. Caching the
# inverse matrix instead of computing will reduce the cost
#
# NOTE: The input matrix is assumed to be invertible!


makeCacheMatrix <- function(x = matrix()) {
    # Matrix wrapper function that allows to hold the matrix as well as
    # its inverse in memory
    #
    # Define an empty inverse of given matrix.
    # set(): Takes the given matrix and defines the inverse as NULL.
    # get(): Returns matrix 
    # setinverse(): Generate inverse matrix
    # getinverse(): Returns inverse matrix
    #
    # NOTE: The input matrix is assumed to be invertible!
    #
    # Args: Matrix to be stored
    #
    # Return: Lists of member functions

    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse 
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    # Calculates the inverse matrix created with the function makeCacheMatrix
    # 
    # Checks if inverse is defined. If defined, prints message that it is using
    # cached data and returns inverse. Otherwise, gets the matrix, solves
    # for the inverse, and sets the inverse
    #
    # Args: A matrix wrapped with makeCacheMatrix
    #
    # Return: Inverse of given matrix


    inverse <- x$getinverse()
    if (!is.null(inverse)){
        message("Using cached data")
    } else {
        org_matrix <- x$get()
        inverse <- solve(org_matrix, ...)
        x$setinverse(inverse)
    }
    inverse
}
