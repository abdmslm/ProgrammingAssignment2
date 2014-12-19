## makeCacheMatrix creates a list containing a function that takes a matrix as an argument to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse the matrix
## get the value of the inverse

makeCacheMatrix <- function(x = matrix(nrow=0, ncol=0)) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The following function calculates the inverse of the matrix created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setsolve function

cacheSolve<- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
