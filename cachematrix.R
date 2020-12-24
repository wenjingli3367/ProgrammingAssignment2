## Put comments here that give an overall description of what your

##The following functions are used to create a special object that stores a matrix and caches its inverse. 

## Write a short comment describing this function
##1.  `makeCacheMatrix`: This function creates a special "matrix" object
#that can cache its inverse, which is a data frame containing a function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #1.  set the value of the matrix 
  set <- function(y) {
  x <<- y
  m <<- NULL
  }
  #2.  get the value of the matrix
  get <- function() x
  #3.  set the value of the inverse of a matrix
  setinverse <- function(inverse) m <<- inverse
  #4.  get the value of the inverse of a matrix
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#2.  `cacheSolve`: This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

