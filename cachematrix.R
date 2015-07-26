## The makeCache function creates a a matrix object which can cache its inverse.
## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## The makeCacheMatrix The first function, makeVector creates a matrix,
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix
## created with the above function. However, it first checks to
## see if the inverse has already been calculated. If so, it gets
## the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}   ## Return a matrix that is the inverse of 'x'
