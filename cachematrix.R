## Two functions will be written, one of them will create an special matrix
## with some auxiliary functions to set and get the data

## makeCacheMatrix creates an special "matrix" with auxiliary functions:
## set -> stores de value of the original matrix
## get -> returns the value of the original matrix
## setinverse -> stores the value of the inverse of the original matrix
## getinverse -> returns the value of the inverse of the original matrix
makeCacheMatrix <- function(x = matrix()) {
  m <<- NULL
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


## cacheSolve returns the value of the inverse of the original matrix if it is cached
## if the cached value is null, it calculates the inverse and stores it in the 
## special "matrix" with set inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
