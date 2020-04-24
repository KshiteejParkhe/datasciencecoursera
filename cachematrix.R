## The functiion makeCacheMatrix() creates objects and stores
## the matrix and its inverse
## While the cacheSolve() function retrives cached inverse matrix
## stored in the former function using or calculates it
## if the stored value is NULL

## This function initializes 'x' and 'i' objects
## These objects are made available in the makeCacheMatrix environment
## i is to be set as inverse matrix in cache later


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Here the function checks whether inverse matrix 'i'
## is available else it calculates the inverse matrix
## using the solve function mentioned

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



Test_matrix <- matrix(c(2,4,6,8), nrow = 2, ncol = 2)
Test <- makeCacheMatrix(Test_matrix)
cacheSolve(Test)
