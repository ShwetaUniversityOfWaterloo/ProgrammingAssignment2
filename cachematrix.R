makeCacheMatrim <- function(m = matrix()) {
  inv_m <- NULL
  set <- function(n) {
    m <<- n
    inv_m <<- NULL
  }
  get <- function() m
  setinverse<- function(inverse) inv_m <<-inverse
  getinverse <- function() inv_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## MakeCacheMatrix creates the matrix and then the inverse of the matrix is returned by cacheSolve 
## If the cached inverse is available, cacheSolve retrieves it, otherwise it computes the cache, and then return it.

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'm'
  inv_m <- m$getinverse()
  if (!is.null(inv_m)) {
    message("getting cached inverse matrix")
    return(inv_m)
  } else {
    inv_m <- solve(m$get())
    m$setinverse(inv_m)
    return(inv_m)
  }
}