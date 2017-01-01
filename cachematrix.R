# Getter and Setter function for Matrix and its Inverse.
makeCacheMatrix <- function(matrix_data = matrix()) {
  Inv <- NULL
  
  set <- function(y) {
    matrix_data <<- y
    Inv <<- NULL
  }
  get <- function() matrix_data
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() Inv
  
  # Returns getter and setter.
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Calculates inverse of a Matrix.
cacheSolve <- function(matrix_data, ...) {
  
  Inv <- matrix_data$getinverse()
  
  # Checks if matrix, of which inverse is calculated,
  # is changed or not.
  if(!is.null(Inv)) {
    message("Getting cached Inverse of Matrix.")
    return(Inv)
  }
  
  # Get the modified/new matrix.
  data <- matrix_data$get()
  
  # Calculates inverse of modified/new matrix.
  Inv <- solve(data, ...)
  
  # Sets inverse so that no need to calculate
  # next time for the same matrix.
  matrix_data$setinverse(Inv)
  
  # Return inverse.
  Inv
}
