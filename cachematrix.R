## Matrix inversion is usually a  time-consuming computation and 
## I write a pair of functions that cache the inverse of a matrix 
## instead of computing it repeatedly. 

## ----------------------------------------------------------
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
## The method first checks to see if the mean has already been calculated.
##    If so,     the method gets the inverse matrix from the cache and 
##               skips the computation. 
##    Otherwise, the method calculates the inverse matrix and sets it 
##               in the cache via the setinv function.
## ------------------------------------------------------------------

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
