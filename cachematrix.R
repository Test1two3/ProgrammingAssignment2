## makeCachematrix creates a special matrix object that can cache its inverse, the function contains 4 functions
##set, get setinverse, getinverse
##set the value of the matrix (overwrites value given with makeCacheMatrix)
##get the value of the matrix
##setinverse -> set the inverse matrix value (not the outcome of the solve function)
##getinverse -> get the value of the inverse matrix (not the outcome of the solve function, 
##will show value of setinverse, if not set will show NULL)

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


## The following function calculates the inverse of the special "matrix" created with the makeCacheMatrix function 
##However, it first checks to see if the inverse has already been set (using the setinverse function) If so, 
## it gets the inverse from the cache and skips the computation. If not, it calculates the inverse of the data and 
##sets the value of the inverse in the cache via the setinverse function.

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
