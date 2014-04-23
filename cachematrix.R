##  makeCacheMatrix
##   Creates a special "matrix", which is a list containing a function to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(z) invMatrix <<- z
    getinverse <- function() invMatrix
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## cacheSolve
##  Calculates the inverse matrix of the special "matrix" x
##  created with the function makeCacheMatrix
##      - first checks to see if the inverse matrix has already been calculated
##      - yes: it gets the inverse from the cache and skips the computation
##      - no:  it calculates the inverse matrix of x
##             and sets the value of the inverse matrix of x in the cache
##             via the setinverse function.

cacheSolve <- function(x, ...) {
    invMatrix <- x$getinverse()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    matrixData <- x$get()
    invMatrix <- solve(matrixData, ...)
    x$setinverse(invMatrix)
    invMatrix
}