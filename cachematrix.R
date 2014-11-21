#creating a matrix object that will cache its inverse  
makeCacheMatrix <- function(m = matrix()) {
  
# setting the inverse property  
  inv_m <- NULL
  
# creating a function to set the matrix  
  set <- function(n) {
    m <<- n
    inv_m <<- NULL
  }

  # Get method will print the matrix   
  get <- function() m
  
# Function to set the inverse of the matrix  
  setinverse<- function(inverse) inv_m <<-inverse
  
# getinverse method will print the inverse of a matrix 
  getinverse <- function() inv_m
  
#list of the methods is returned  
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
    message("getting the cached inverse matrix")
    return(inv_m)
  } else {
    inv_m <- solve(m$get())
	
# setting the inverse 	
    m$setinverse(inv_m)
	
# Return the matrix	
    return(inv_m)
  }
}