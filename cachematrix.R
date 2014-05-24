## makeCacheMatrix and cacheSolve will work together to cache the inverse of a matrix 
## thereby saving some computation time.
 
 
## makeCacheMatrix creates a matrix and a list of functions. 
## These functions set or get the value of the matrix, and 
## set or get the inverse of this matrix assuming that the matrix is invertible. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
}


## cacheSolve first checks if the inverse of a given matrix can be found in cache
## if yes, it will return it without computation, if no it will compute it and cache it

cacheSolve <- function(x, ...) {
  i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
  }
}
