## The following functions calculate the inverse of a matrix, and then save it in the cache
## if an inverse has already been saved, the saved value is returned instead of doing a new calculation.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i           
  ## Return a matrix that is the inverse of 'x'
}
