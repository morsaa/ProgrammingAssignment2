## makeCacheMatrix creates a list to be used by cacheSolved.
## The list contains 4 functions:
## 1. set: sets the matrix x
## 2. get: gets the matrix x
## 3. setinverse: sets the inverse matrix of x
## 4. getinverse: gets the inverse matrix of x

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## x here is makeCacheMatrix(y), where y is the matrix we want to invert.
## cacheSolve first checks to see if the inverse has already been computed.
## If the inverse is in the cache, it returns the inverse. Otherwise, it
## calculates the inverse and sets it in the cache, then returns the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, diag(dim(data)[1]), ...)
        x$setinverse(m)
        m
}
