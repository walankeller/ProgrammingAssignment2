## Put comments here that give an overall description of what your
## functions do
## two functions: 
#   1-makeCacheMatrix, takes a matrix and caches it, and 
#   2-cacheSolve(), returnes the cached value of the inverted matrix 
#     or, calculates and returns the inverse of a matrix if it hasn't already been cached
#note: example calls at the bottom

## Write a short comment describing this function
# Set and Get the value of the matrix
# Set and Get the inverse value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  matrixval <- NULL
  set <- function(y) {
    x <<- y
    matrixval <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) matrixval <<- inv
  getinverse <- function() matrixval
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

## Write a short comment describing this function
# ak cacheSolve: Calc the inverse of a matrix.
# ak return the original matrix if the matrix has already been calculated
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  # Cache the inverse matrix
  x$setinv(inv)
  # Return the inverse matrix
  inv
}

# ## example call:
# # # Create a matrix x / 
# # x <- matrix(rnorm(64), nrow = 8)  #8x8
#  x <- matrix(rnorm(16), nrow = 4)  #4x4
# # x <- matrix(rnorm(4), nrow = 2)   #2x2       
# # Create our special matrix
# cx <- makeCacheMatrix(x)                  
# # original matrix
# cx$get()                                  
# # matrix inverse
# cacheSolve(cx)      


# adding the following hw assignment instructions:
# 1)Fork the GitHub repository containing the stub R files at https://github.com/rdpeng/ProgrammingAssignment2 to create a copy under your own account.
# 2)Clone your forked GitHub repository to your computer so that you can edit the files locally on your own machine.
# 3)Edit the R file contained in the git repository and place your solution in that file (please do not rename the file).
# 4)Commit your completed R file into YOUR git repository and push your git branch to the GitHub repository under your account.
# 5)Submit to Coursera the URL to your GitHub repository that contains the completed R code for the assignment.
