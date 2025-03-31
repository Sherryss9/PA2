## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse
  inv <- NULL

  ## define the set function to assign new
  set <- function(y) {
    x <<- y

   ## if there is a new matrix, reset inv to NULL
    inv <<- NULL
  }

  ## define the get fucntion - returns value of the matrix argument
  get <- function() x

  ## assigns value of inv in parent environment
  setinverse <- function(inverse) inv <<- inverse

  ## gets the value of inv where called
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## 3) Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Get the matrix from the object
    data <- x$get()

    ## Calculate the inverse 
    inv <- solve(data, ...)

    ## Set the inverse to the object
    x$setinverse(inv)

    ## Return the matrix
    inv
}

   
