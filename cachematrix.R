## The two functions below utilizes the <<- operator to create a special object that stores 
## a Matrix and cache's its Inverse.

## makeCacheMatrix creates a special object, which is a list containing 
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the Inverse of the matrix
##    get the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set a new matrix to the special "matrix" and clear the inverse
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## returns the existing matrix
  getmatrix <- function() x
  
  ## computes and updates the inverse using solve()
  setinverse <- function(solve) inv <<- solve
  
  ## returns the inverse 
  getinverse <- function() inv
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve calculates the Inverse of the matrix created with the above function. 
## It first checks to see if the Inverse has already been calculated. 
## If so, it gets the Inverse from the cache and skips the computation. 
## Otherwise, it calculates the Inverse and sets the value of the Inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## If inverse is available use that cached value.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If inverse is not available, compute it.
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
