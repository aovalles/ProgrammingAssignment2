## makeCacheMatrix creates a container for a matrix object
## and so creates a new type of matrix.
## Tne new Matrix allows the inverse to be cached for reuse and
## has special functions to set and get the values of the matrix
## and to set and get the inverse.
##
## cacheSolve creates a CacheMatrix object and shows how
## to use the set and get functions.


## Make the new cacheble type of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(matrx) {
        x <<- matrx
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(InversedMatrix) inv <<- InversedMatrix
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## Implements the new type of matrix and shows how to use the new type
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
     
}
