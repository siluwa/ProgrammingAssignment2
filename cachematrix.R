## Put comments here that give an overall description of what your
## functions do
## The goal of this programming is to establish the function pairs of
## "makeCacheMatrix" and "cacheSolve" to cache the inverse of a matrix

## Write a short comment describing this function
## This function serves to creates a special "matrix", which is really a list
## containing a function to set and get the value of the matrix, as well as set
## and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function()
                x
        setinv <- function(inverse)
                inv <<- inverse
        getinv <- function()
                inv
        ## create list with methods for get/set of both original matrix and
        ## its inverse, return the list to the parent environment
        ## note that this technique enables use of $ operator to access
        ## each function from the list
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## The following function computes the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been obtained. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache via the "setinv"function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
## testing
x <- matrix(rnorm(16), nrow = 4,ncol = 4)
z<- makeCacheMatrix (x)
z$get()
cacheSolve(z)
