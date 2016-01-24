## This is the Programming Assignment 2 of the Coursera "R Programming" course

## This exercise demonstrates the usage of the "<<-" operator that can help
## store an object's state that is "persistent". This is achieved with the use
## of the R scoping rules.


## This function creates an object (list) that stores the matrix value passed
## as a parameter and provides functions to get/set the matrix and its inverse.

## Parameter: x - matrix object.
## Returns: a list with functions corresponding to get/set operations of the
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function accepts an object constructed by makeCacheMatrix function
## above and calculates and returns the inverse of the matrix stored in that
## object. It checks if the inverse of the matrix has been already calculated
## and returns the cached value in this case. Otherwise the result is computed
## using function "solve" and stored in the parameter object.

## Parameter: x - an object constructed by the function makeCacheMatrix above.
## Returns: the inverse of the matrix stored in x. As a side effect, the inverse
## is also stored in x. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (is.null(inv)) {
        ## cached value is NULL, calculating the inverse
        mat <- x$get()
        inv <- solve(mat)
        x$setinv(inv)        
    }
    ## Return a matrix that is the inverse of 'x'
    inv
}
