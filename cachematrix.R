## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The function creates a matrix object which includes 
# a list containing functions which can set and retrieve the cache values for a matrix
# and its inverse. The syntax is 
# x$get() to get the matrix
# x$set(y) to set the matrix
# x$getinverse() to get the matrix inverse
# x$setinverse(yinv) to set the matrix inverse

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


## Write a short comment describing this function
# cacheSolve tries to read the inverse from the cache. If the values has been set, 
# it will return it and exit. If the values has not been set, it will apply solve 
# on the matrix, set the value of the inverse in the cache and return the value.

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
