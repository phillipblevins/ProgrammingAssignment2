## These function create a special cacheable matrix and  solve for the inverse 
## when solving for the inverse if possible it uses any cached inverse

## Creates a special matrix that will allow the inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse  <- function(inverse ) m <<- inverse 
  getinverse  <- function() m
  list(set = set, get = get,
       setinverse = setinverse ,
       getinverse  = getinverse )
}


## Solves for the inverse of the matrix. If the inverse as already been taken it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse ()
  if(!is.null(m)) {
    message("getting cached inverse data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse (m)
  m
}
