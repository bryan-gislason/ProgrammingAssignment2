## To get the inverse matrix while saving computation power,
## construct a special matrix with makeCacheMatrix,
## (e.g. cm <- makeCacheMatrix(matrix(rnorm(1:25, 5, 5))) ),
## pass it to cacheSolve,
## cacheSolve(cm), which will return the cached version
## or recalculate the inverse if the matrix has changed

## makeCacheMatrix handles setting and getting a
## matrix and its inverse using environment scopes
makeCacheMatrix <- function(x = matrix()) {

    matrixInverse <- NULL
    set <- function(y) {
        x <<- y
        matrixInverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrixInverse <<- inverse
    getinverse <- function() matrixInverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Gets the cached inverse of a square matrix
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
