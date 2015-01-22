## These two functions together can be  used to store cached values of the inverse of a matrix. 
## The first time cacheSolve is called it calculates the inverse of the matrix, the second time it is called it will retrieved the cached inverse.

## As a demonstration of the functionality of the below functions, simply call:
## testmatrix <- matrix(c(1,2,3,4), nrow=2, ncol=2)     # creates a test matrix
## cacheMatrix <- makeCacheMatrix(testmatrix)           # creates a special cache copy of the matrix
## cacheSolve(cacheMatrix)                              # returns the inverse of the matrix by actually using the "solve" function
## cacheSolve(cacheMatrix)                              # the second time, third time, etc. it is called, it will return the cached copy



## used to make a special copy of a matrix that can then have the values and inverse stored in cache memory. 
## it accepts a standard matrix, and returns a list with the following elements

## set = sets the value of the matrix
## get = gets the value of the matrix
## setinv = sets the cached value for the matrix
## getinv = gets the cached value for the matrix

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


## used to compute the inverse of a matrix. if there is a cached inverse it will retrieve that, otherwise it will calculate 
## and store the new inverse matrix. it accepts a list made by the makeCacheMatrix function, and returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        message("no cached data proceed to inverse matrix")
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
