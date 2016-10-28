## Contains functions for Coursera course R Programming Week 3
## Programming Assignment 2.


## ******************************************************************
## Function: makeCacheMatrix
## ******************************************************************
## Parameters:
##
## m - a matrix
## ******************************************************************
## Description:
##
## Creates a matrix with list functions for Get, Set, GetInverse,
## and SetInverse.
##
## ******************************************************************
## List Functions:
##
## Get:
##
## Returns the matrix m.
## 
## Set:
##
## Sets the matrix m.  Also clears the inversedMatrix.
##
## GetInverse:
##
## Returns the matrix stored in inversedMatrix.
##
## Set Inverse:
##
## Sets the matrix inversedMatrix.
##
## ******************************************************************

makeCacheMatrix <- function(m = matrix()) {
  ## This is the inversed matrix
  inversedMatrix <- NULL
  
  ## Define Set, Get, SetInverse and GetInverse methods or functions of 
  ## this object.
  
  ## Set also clears the inversed matrix.
  Set <- function(y) {
    m <<- y
    inversedMatrix <<- NULL
  } ## end Set
  
  ## Gets the matrix stored in m.
  Get <- function() m
  
  ## Stores the matrix passed in as solvedMatrix to the parent inversedMatrix.
  SetInverse <- function(solvedMatrix) inversedMatrix <<- solvedMatrix
  
  ## Returns the matrix stored in inversedMatrix.
  GetInverse <- function() inversedMatrix
  
  ## Add the functions to a list to be accessable as methods of the object.
  list(Get = Get, Set = Set, SetInverse = SetInverse, GetInverse = GetInverse)
} ## end makeCacheMatrix


## ******************************************************************
## Function: cacheSolve
## ******************************************************************
## Parameters:
##
## x - a "special" makeCacheMatrix matrix. 
## ******************************************************************
## Description:
##
## Inverses the matrix x and caches the result in the makeCacheMatrix
## matrix.
##
## ******************************************************************

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the cached version of the inverse matrix, if found.
  inversedMatrix <- x$GetInverse()
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  } ## end if
  
  ## If the cached version is null, compute the inverse matrix.
  ## Get the original matrix
  originalMatrix <- x$Get()
  
  ## Inverse the original matrix my solving it.
  inversedMatrix <- solve(originalMatrix, ...)
  
  ## Cache the inversed matrix.
  x$SetInverse(inversedMatrix)
  
  ## Return the inversed matrix.
  inversedMatrix
} ## end cacheSolve
