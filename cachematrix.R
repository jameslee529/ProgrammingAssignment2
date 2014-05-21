## Matrix inversion is usually a  time-consuming computation and 
## I write a pair of functions that cache the inverse of a matrix 
## instead of computing it repeatedly. 

## ----------------------------------------------------------
#  makeCacheMatrix: return a list of functions
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
## ----------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## ------------------------------------------------------------------
## Compute the inverse of the matrix. If the inverse has already been
## calculated, it returns the cached inverse matrix.
## ------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinv(inverseMatrix) 
  
  inverseMatrix
}
