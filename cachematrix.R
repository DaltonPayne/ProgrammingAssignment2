# This function makes a special type of matrix that can cache its own inverse for future reference
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL  # Initialize 'inv' as NULL. This variable will hold the cached inverse of the matrix 'x'.
  
  set <- function(y) {  # A function to set the value of the matrix 'x'
    x <<- y  # The <<- operator is used to assign value 'y' to 'x' in the parent environment (outside of this function)
    inv <<- NULL  # When 'x' is updated, 'inv' is reset to NULL because the cached inverse no longer corresponds to 'x'
  }
  
  get <- function() x  # A function to get the value of the matrix 'x'
  
  setInverse <- function(inverse) inv <<- inverse  # A function to set the cached inverse of the matrix
  
  getInverse <- function() inv  # A function to get the cached inverse of the matrix
  
  # The function returns a list of functions that can set/get the value of 'x' and set/get the cached inverse of 'x'
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function calculates the inverse of the special "matrix" returned by 'makeCacheMatrix'. 
# If the inverse has already been calculated (and the matrix has not changed), then the 'cacheSolve' should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()  # Try to get the cached inverse of 'x'
  
  # If 'inv' is not NULL, that means the cached inverse of 'x' was found
  if (!is.null(inv)) {
    message("getting cached data")  # Print a message indicating that cached data is being used
    return(inv)  # Return the cached inverse
  }
  
  # If 'inv' is NULL, that means the inverse has not been calculated yet or the matrix 'x' was changed
  data <- x$get()  # Get the value of the matrix 'x'
  
  inv <- solve(data, ...)  # Calculate the inverse of 'x'
  
  x$setInverse(inv)  # Set the cached inverse to the newly calculated inverse
  
  inv  # Return the inverse of 'x'
}
