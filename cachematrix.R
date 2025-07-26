## Writing a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" 
#object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {inv <- NULL

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


## This function computes the inverse of the special
#"matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {inv <- x$getinverse()

if (!is.null(inv)) {
  message("getting cached inverse")
  return(inv)  # Return cached inverse
}

mat <- x$get()
inv <- solve(mat, ...)  # Compute the inverse
x$setinverse(inv)       # Cache it
inv   ## Return a matrix that is the inverse of 'x'
}
