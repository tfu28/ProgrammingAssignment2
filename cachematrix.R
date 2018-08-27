# These functions aim to cache the inverse of a matrix


# makeCacheMatrix takes a matrix as input and caches it and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y = matrix()) {
    
    x <<- y
    
    inv <<- NULL
    
  }
  
  get <- function() x
  
  setInv <- function(inv_initial) inv <<- inv_initial
  
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}



# cacheSolve takes a matrix as input and returns the inverse of the matrix
# if the inverse is in the cache, cacheSolve returns from the cache
# otherwise, cacheSolve calculates the inverse

cacheSolve <- function(x, ...) {
  
  # returns the inverse of the matrix x
  
  inv <- x$getInv()
  
  if(!is.null(inv)) {
    
    message("getting cached data")
    
    return(inv)
    
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setInv(inv)
  
  inv
  
}
