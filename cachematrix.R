## Put comments here that give an overall description of what your
## functions do Inverse of a Matrix

## Write a short comment describing this function
##1.  `makeCacheMatrix`: This function creates a special "matrix" object
#that can cache its inverse, which is a data frame containing a function
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse of a matrix
#4.  get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#2.  `cacheSolve`: This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m
}

