
## makeCacheMatrix returns a list containing 4 functions:
## 1. Function to set the value of the square matrix
## 2. Function to get the value of the matrix
## 3. Function to set the value of the inverse of the matrix
## 4. Function to get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the cached data for the inverse of a matrix if it has been already calculated
## Otherwise, it calculates the inverse and asigns it to be retrieved later

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)){
    message("getting cached data for the inverse of the matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

