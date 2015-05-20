## The 2 functions below are used to create a spl object that stores a matrix
## and cache's its inverse

## This function is a list to 1. set the value of matrix; 2. get the value of matrix
## 3. set the value of its inverse; 4. get the value of its inverse
## Please note inv_x variable is used as a variable to store the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv_x = NULL
    set <- function(y) {
      x <<- y
      inv_x <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(res_matrix) { 
        inv_x <<- res_matrix
    }
    getinverse <- function() {
        return(inv_x)
    }
    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## Computes the Inverse of matrix, only if there is no cache in memory
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_x <- x$getinverse()
    
    if (!is.null(inv_x)) {
      message("getting cached data")
      return(inv_x)
    }
    else {
      data <- x$get()
      inv_x <- solve(data, ...)
      x$setinverse(inv_x)
      return(inv_x)
    }
} 
