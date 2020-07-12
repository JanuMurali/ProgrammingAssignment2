## This is a program to cache the inverse of a matrix using two functions;
## makeCacheMatrix and cacheSolve

## makeCacheMatrix consists of functions to set the values of matrix and mean and 
## retrieve those values

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve first checks if inverse of the matrix has already been computed,if so it
## gets cached value,otherwise computes and returns the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m     ## Return a matrix that is the inverse of 'x'
  
}