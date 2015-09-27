## this function works like a class, it creates a list
## that contains 4 member functions: set, get, setInv
## and getInv. it uses <<- assignment operator so that
## these internal variables are not exposed to the
## outside environment.

## This function creates a matrix that can store its
## own inverse for faster computations later

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setINV <- function(inverse) inv <<- inverse
  getINV <- function() inv
  list(set = set, get = get,
       setINV = setINV,
       getINV = getINV)

}


## This function computes the inverse of the special "matrix" 
## created by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getINV()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setINV(inv)
  inv
}
