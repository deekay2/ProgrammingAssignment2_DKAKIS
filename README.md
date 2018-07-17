# ProgrammingAssignment2_DKAKIS
Assignment 2 on Lexical Scoping

# Programming Assignment 2: Lexical Scoping

# Cache function for the inverse of a matrix
# The function will need to do the following:
    # set the value of the matrix
    # get the value of the matrix
    # set the value of the inverse
    # get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The next function computes the inverse of the special “matrix” returned 
# by the function above (makeCacheMatrix). 
# If inverse has been computed (and the matrix hasn't changed), 
# then cacheSolve gets the inverse from the above function.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# Testing the functions:

z <- matrix(c(2:5),2,2)
z
z1 <- makeCacheMatrix(z)
cacheSolve(z1)
