## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get=get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Write a short comment describing this function
## Take a object which can cache inverse matrix result. The underlying object
## either calculate inverse for a new matrix input, or load cached result if the 
## inverse has been calculated once

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m  
}
