## A pair of functions which together provide functionality that allows for the value of a matrix inverse to be cached for later reuse
## When the values of the matrix change, the cached inverse is cleared and set to NULL
## makeCacheMatrix - returns a list object that functions as a matrix
## the values of the 'matrix' can be set, retrieved, and inversed through the functions in this list
## cacheSolve - takes a 'matrix' object created by makeCacheMatrix() as argument, and returns the cached inverse value if it exists, or calculates the inverse value and stores it in the cache 

## Function: makeCacheMatrix
## argument: x - an n by n square matrix that can be inversed
## return value: a list containing functions that operate on x, or its inverse i:
#### set: sets the value of x and sets i to NULL
#### get: returns the value of x
#### setInverse: sets the value of i 
#### getInverse: returns the value of i 

makeCacheMatrix <- function(x = matrix()) {

		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Function: cacheSolve
## argument: x - a list object as returned by the makeCacheMatrix function
## cacheSolve checks for the presence of a cached inverse value
## if there is a cached value, the value is returned
## if there is no cached value, the inverse of the matrix represented by x is calculated, set, and returned

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}


