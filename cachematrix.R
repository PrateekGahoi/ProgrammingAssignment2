## The two functions below utilizes the <<- operator to create a special object that stores 
## a Matrix and cache's its Inverse.

## makeCacheMatrix creates a special object, which 
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the Inverse of the matrix
##    get the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve calculates the Inverse of the matrix created with the above function. 
## It first checks to see if the Inverse has already been calculated. 
## If so, it gets the Inverse from the cache and skips the computation. 
## Otherwise, it calculates the Inverse and sets the value of the Inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
