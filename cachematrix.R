



## General Usage:
## create a special matrix using the makeCachMatrix function
## Pass the special matrix from the makeCacheMatrix to the cacheSolve function
## * assumption is that the matrix supplied is invertible
## 
## 

## Invocation:
## amatrix <- matrix(c(1, 2, 3, 4), nrow = 2,  ncol = 2)
## x <- makeCacheMatrix(amatrix)
## cacheSolve(x)
##  
## cacheSolve(makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2,  ncol = 2)))

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

## return a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inv <- NULL
    
    # Set function for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get function  for the matrix
    get <- function() x
    
    # set function for the inverse
    setinv <- function(inverse) inv <<- inverse
    
    # get function for the inverse
    getinv <- function() inv
    
    # Return the matrix with functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve: 
## Compute the inverse of the matrix. If the inverse is already
## calculated before, it returns the cached inverse.
## (assumption is that the matrix supplied is invertible)

##  Note. Possibly use determinate function to throw an error if the 
##  matrix is not invertible-- det(matrix))
##  if 0, fail --> not included in function below
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # calculate the inverse since it wasn't cached
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the inverse by calling setinv
    x$setinv(inv)
    
    # Return the inverse
    return (inv)
}
