makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y       # Set the matrix
    inv <<- NULL  # Clear cached inverse when matrix changes
  }
  
  get <- function() x                # Get the matrix
  setinverse <- function(inverse) inv <<- inverse  # Cache the inverse
  getinverse <- function() inv      # Retrieve the cached inverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)  # Return cached inverse
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse
  x$setinverse(inv)       # Cache it
  inv
}
