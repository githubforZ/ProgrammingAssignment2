## makeCacheMatrix will create a list of functions corresponding to the input matrix that 
## can be processed using cacheSolve


## makeCacheMatrix sets the value of martrix x, gets the value of x, sets the value of the 
## inverse (m), and gets the value of the inverse 


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##  cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

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


