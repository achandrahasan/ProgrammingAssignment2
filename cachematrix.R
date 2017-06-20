

## makeCacheMatrix function creates a special matrix object that can cache its inverse. Also
## is a list containing functions to ser a matrix, to get a matrix, set the inverse of the matrix and to get
## the inverse of matrix which are exposed to the public. 
makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function returns the inverse of a matrix returned by makeCacheMatrix function. 
## It uses the inverse from cache if it has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
