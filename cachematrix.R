## These functions in conjunction take a matrix and find the inverse of it, once it is found once it is cached using the setinverse function and can be retrieved later by the getinverse function without needing to redo the calculations.

## makeCacheMatrix() is used primarily to create a "setter" and a "getter" so that the inverse can be stored and retrieved without calculations later on. The last line names the functions and puts them in a list so they can be used later by name and not indexing by location.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function starts by checking if an inverse is cached already, if it is, it sends a message and retrieves it, if not it retrieves the data needed from the previous function, finds the inverse, sets it for future cases, and returns the inverse.

cacheSolve <- function(x, ...) {
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
