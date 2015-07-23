## Assignment 2: Caching the Inverse of a Matrix

## Run the following code on the prompt to test the functions.
## The result should be the same as solve(A)

## A <- matrix(c(3, -4, 2, -5), nrow=2, ncol=2, byrow=TRUE)
## B <- makeCacheMatrix(A)
## cacheSolve(B)


## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## returns a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
## This list is used as the input to cacheSolve()

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

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
	      invMat <- x$getinv()
        if(!is.null(invMat)) {
                ##If inverse is already calculated then retrieve it from the cache
                message("getting cached data")
                return(invMat)
        }
	      ##Or else compute the inverse
        data <- x$get()
        invMat <- solve(data, ...)
        ## Sets the value of inverse in the cache
        x$setinv(invMat)
        invMat
}
