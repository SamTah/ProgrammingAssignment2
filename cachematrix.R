## Assignment 2: Caching the Inverse of a Matrix

## Run the following code on the prompt to test the functions.
## The result should be the same as solve(A)

## A <- matrix(c(3, -4, 2, -5), nrow=2, ncol=2, byrow=TRUE)
## B <- makeCacheMatrix(A)
## cacheSolve(B)


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  invMat <- NULL
        set <- function(y) {
                x <<- y
                invMat <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) invMat <<- inverse
        getinv <- function() invMat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  invMat <- x$getinv()
        if(!is.null(invMat)) {
                message("getting cached data")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data, ...)
        x$setinv(invMat)
        invMat
}
