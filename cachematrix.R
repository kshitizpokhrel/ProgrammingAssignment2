## Put comments here that give an overall description of what your


## makeCacheMatrix function gets the matrix from user and set the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function
## cachSolve function checks to see if the inverse is available in the cache
##if it is in cache, it gets it from there without doing any further calculations
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
