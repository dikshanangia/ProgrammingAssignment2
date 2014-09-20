## The below functions take a matrix as an argument to the
##  matrix and calculate the inverse of the matrix


## This function creates a matrix and assigns default values to the variables

makeCacheMatrix <- function(x = matrix()) {
{
  m <- NULL
  set <- function(y) 
    {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

}


## This function takes the value of the matrix from the makeCacheMatrix function and calculates the inverse of a matrix

cacheSolve <- function(x, ...) {
       m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
