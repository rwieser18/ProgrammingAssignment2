## makeCacheMatrix creates a special matrix which is a list containing four functions
## which set the value of the matrix, get the value of the matrix, set the value of the inverse,
## and get the value of the inverse. 


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setinv <- function(Solve){
    inverse <<- solve
  }
  getinv <- function() {
    inverse
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv) # return list of functions
}



cacheSolve <- function(x, ...) {
  inverse <- x$getinv() # checks to see if the inverse has already been calculated
  if(!is.null(inverse)) {   # if it has already been cached
    message("getting cached data")
    return(inverse)  # return the cached inverse and exit the program
  }
  data <- x$get() # if it hasn't, put the data in 'data'
  inverse <- solve(data, ...) # compute the inverse of the data
  x$setinv(inverse) # cache the result
  inverse # return a matrix that is the inverse of x
}
