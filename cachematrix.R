## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The makeCacheMatrix creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL #set initially a null
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse #pass the function
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    m <- x$getinverse() #gets the getinverse from the list
    if(!is.null(m)) { #if it is not empty, gets the cached data
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setinverse(m)
    m
}