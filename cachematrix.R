## These functions comprise a cached matrix and its inverse, and a function to compute
## the matrix inverse and store it, such that
## the inverse can be computed once and stored with the original for
## future lookup
## This was based on the Vector example provided with the programming assignment
## On initial call, make a cached matrix and store the original matrix in 
## the environment.
## The inverse can be set at a later point

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Called with the object returned from makeCacheMatrix.  
## If the object does not have inverted matrix
## calculated, compute it and set it back on the object, 
## otherwise return the previously computed cached
## inverted matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
    	    message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
