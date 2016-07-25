## The makeCacheMatrix and the cacheSolve environment exhibit caching in a different 
## environment

## makeCacheMatrix creates the relevant get and set functions for returning matrix
## and the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) {
    i <<- solve
    print("in setting")
  }
  getsolve <- function() i
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function checks if the the inverse already exists and returns the same
## if exists, else creates inverse and returns

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    print("getting cached matrix")
    return(i)
  }
  print("getting new inverse")
  data <- x$get()
  print(data)
  i <- solve (data, ...)
  x$setsolve(i)
  i
}
