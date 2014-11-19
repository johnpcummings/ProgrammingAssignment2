## The combination of function creates a inverse of a matrix
## and stores the inverse in cache for possible future use.

## The makeCacheMatrix creates and stores the inverse of matrix

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


## Runs Program to get inverse matrix.  If a previous inverse matrix
## was completed it uses the previous inverse and if no inverse marix
## was created, the program creates the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m = x$getinverse()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
