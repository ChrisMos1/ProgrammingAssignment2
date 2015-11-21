## 2 functions to store the inverse in external storage (makeCacheMatrix) and
## to calculate it if not yet stored (cacheSolve)

## makeCacheMatrix provides a toolset to put and get a matrix to and from an "external" 
## storage and get and set (store) the inverse of the matrix. 
##!! Attention: getinverse returns the value from storage, but does not calculate
## it! It is only recalculated whith cacheSolve (see below). If calling data is changed, 
## inverse must be calculated again!

makeCacheMatrix <- function(x = matrix()) {
    # always create m locally 
    m <- NULL
    # (set is unnecessary for assignment, we only need to save inverse, but not the matrix itself)
    set <- function(y) {
        x <<- y
           m <<- NULL
    }
    get <- function() x
    setinverse <- function(inve) m <<- inve
    getinverse <- function() m
    list(#set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of matrix x and stores it in 
## "external" storage via setinverse (from makeCacheMatrix). If the inverse already
## exists, it is just returned as matrix. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
