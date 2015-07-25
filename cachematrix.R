##this project contains two functions that can be used to cache the inverse of the matrix.


## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of following functions:
## 1. set the value of the matrix. 2. get the value of the matrix
## 3. set the value of the inverse of the matrix. 4. get the value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function()s
  list(set = set, get = get, setinverse = setsolve, getinverse = getsolve)
}



## this function computes the inverse of the special "matrix" returned by "makeCacheMatrix" above. 
## if the inverse has already been calculated (if(!is.null(s))), then retrive the inverse from the cache. 
## otherwise, it computes the inverse of the matrix and set the value of inverse in the cache via "setinverse" function.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)){
    message("retrieve the inverse from the cache")
    return(s)
  }
  else {
    data <- x$get()
    s <- solve(data, ...)
    x$setinverse(s)
    s
  }
}