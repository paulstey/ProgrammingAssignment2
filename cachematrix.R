###########
# Coursera R Programming
# Assignment 2
# Date: July 14, 2015
# Author: Paul Stey
###########



####
# The two functions below are useful for storing a matrix's 
# inverse along with the orginal matrix. This save computational
# effor for tasks that require inverting the same matrix more 
# than once.
####




####
# This function creates a "matrix" object that will be capable 
# of storing the inverse of the matrix, too.
####

makeCacheMatrix <- function(X = matrix()){
   
   Xinv <- NULL
   
   set <- function(Y){
      
      X <<- Y
      Xinv <<- NULL
   }
   
   get <- function() X
   
   setinv <- function(X_inverse) Xinv <<- X_inverse
   
   getinv <- function() Xinv
   
   list(
      set = set, 
      get = get,
      setinv = setinv,
      getinv = getinv
   )
}







####
# Given a "matrix" object created by the makeCacheMatrix()
# function above, this function inverts the matrix, and 
# stores the inverted matrix in the orginal object.
####

cacheSolve <- function(X, ...){
   
   Xinv <- X$getinv()
   
   if(!is.null(Xinv)){
      
      message("getting cached data")
      
      return(Xinv)
   }
   
   Xmat <- X$get()
   
   Xinv <- solve(Xmat, ...)
   
   X$setinv(Xinv)
   
   Xinv
}






