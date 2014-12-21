## Author : Jaswanth Yella
## Date   : 21/12/2014
## Program: Caching the Inverse of a Matrix

## The following program caches the Matrix inversion of a provided matrix.
## This is primarily achieved with two functions.
## makeCacheMatrix() and cacheSolve() Functions

## -------------------------------------------------------------------------

## makeCacheMatrix function takes any valid square matrix as a parameter and
## saves the matrix and it is inverse. If the inverse is not available, the 
## caching of the matrix inverse can be performed with cacheSolve() function7

makeCacheMatrix <- function(x = matrix()) {
  
  # Check if the provided matrix has rows = columns i.e., a square matrix
  # Else Matrix Inverse will fail.
  
  if( ncol(x)!=nrow(x)) {
        print("Error: Rows and Columns must be equal. Please provide a valid square matrix")
  }
  
  # Initially the matrixInverse variable is set to NULL  
  matrixInverse <- NULL
  
  # The setter function saves the matrix passed as argument to the function.
  set <- function(y){
     x <<- y
     matrixInverse <<- NULL
  }
  
  # Upon calling the getter function, the matrix elements can be viewed
  get <- function() x
  # SetMatrixInverse function sets the inverse of the matrix
  setmatrixInverse <- function(minverse) matrixInverse <<- minverse
  # GetMatrixInverse function gets the inverse of the matrix
  getmatrixInverse <- function() matrixInverse
  
  # Functions are listed in the list.
  list(set=set, 
       get = get, 
       setmatrixInverse = setmatrixInverse, 
       getmatrixInverse = getmatrixInverse)

}


## CacheSolve Function gets the makeCacheMatrix and computes the inverse.
## The Caching the Inverse of a Matrix is achieved at this stage.

cacheSolve <- function(x, ...) {
  
  # GetMatrixInverse and loads to variable i
  i <- x$getmatrixInverse()
  
  # If i is not null, then it means the inverse is available. So this will
  # print the matrix and exits the function
  if(!is.null(i)){
      message("getting cached data")
      return(i)
  }
  
  # If inverse matrix is null, then makeCacheMatrix is loaded to mdata variable
  
  mdata <- x$get()
  
  # Using solve, the inverse is computed and loaded to i variable
  i <- solve(mdata, ...)
  
  # Using setter function, the inverse is cached.
  x$setmatrixInverse(i)
  
  #Finally, the inverse of the matrix will be printed
  i
}


## -------------------------------------------------------------------------

## TestCases For Caching the Inverse of a Matrix

# TestCase-1: Prints the inverse matrix
testMatrix <- matrix(1:4,2,2)
cachematrix <- makeCacheMatrix(testMatrix)
cacheSolve(cachematrix)

# TestCase-2: Fails Caching the inverse matrix
testMatrix <- matrix(1:10,5,2)
cachematrix <- makeCacheMatrix(testMatrix)
cacheSolve(cachematrix)
