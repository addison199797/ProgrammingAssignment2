##the two functions work together to inverse a matrix and store it in cache

##makeCacheMatrix includes 4 functions
##2 to get and set the initial matrix and 2 to get and cache the inversed matrix

makeCacheMatrix <- function(x = numeric()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixInverse) inverse <<- matrixInverse
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##return the inverse of a matrix by taking in the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setinverse(inverse)
  inverse
}
