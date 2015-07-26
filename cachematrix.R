# Usage:
# > x <- matrix(runif(16), 4, 4)              // Create a matrix x 4x4 
# > cm <- makeCacheMatrix(x)                  // Create our special matrix
# > cm$get()                                  // Get the matrix
# > cacheSolve(cm)                            // Get the inverse
# > cacheSolve(cm)                            // Return the cached inverse


# makeCacheMatrix: return a list of functions:
# Setter the value of the matrix
# Getter the value of the matrix
# Setter the value of the inverse
# Getter the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # inv is the cached inverse matrix
  inv <- NULL
  
  # Setter
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Getter
  get <- function() x
  
  # Setter inverse
  setinv <- function(inverse) inv <<- inverse
  
  # Getter inverse
  getinv <- function() inv
  
  # Return the functions matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}



# cacheSolve: Compute the inverse of the matrix. If the inverse has already been
# calculated, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # If the inverse is calculated, we return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  

  # IF The inverse is not yet calculated, then we calculate it
  data <- x$get()
  inv <- solve(data)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return inverse matrix
  inv
}