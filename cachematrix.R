## Put comments here that give an overall description of what your
## functions do
## This functions saves and retrieves the matrix and inverse of the matrix. The inverse of the matrix is stored in the cache so that 
## if the function is called with the same input again, instead of calculating the inverse of the matrix again, it retrieves the result from the cache.
## The makeCacheMatrix receives a matrix as an argument, it has another four functions namely set, get, setinverse, and getinverse
## set, saves the matrix and get retrieves the matrix. setinverse and getinverse, saves and retrieves the inverse of the matrix
  
makeCacheMatrix <- function(x = matrix()) {
  m   <- NULL
  set <- function(y) {
    y
    x   <<- y
    m   <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list( set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function
## The cacheSolve function recieves the matrix and check whether the input is same as any of the previous inputs, and if the input is same, the result is retrieved from the cache rather than re calculating to improve the efficiency 
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 
   m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- matrix(x$get(),2,2)
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

