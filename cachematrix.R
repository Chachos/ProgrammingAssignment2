## These functions take in an invertible square matrix and calculate the inverse matrix. 
## The matrix and its inverse are then cached for later use. The functions enable more
## efficient use of computer memory by decreasing the need for incessant calculation of 
## the inverse of a matrix under consideration.

## The makeCacheMatrix function initializes the inverse matrix cache and sets the parameters
## to be used in the inverse matrix calculation. The output of the function is a list that 
## contains a cached matrix and its inverse. 

   makeCacheMatrix <- function(mat = matrix()){
#   initialize the global variable
    invMat <- NULL
#   list object that sets and gets both the input and inverse matrices
    list( 
#   initialize the input and inverse matrices
    set = function(X) {
        mat <<- X
        invMat <<- NULL
    },
    setInverse = function(Y) {
            invMat <<- Y
    },
#   get the input matrix object
    get = function() {
        mat
    },
#   get the inverse matrix object
    getInverse = function() {
        invMat
    }
    )
}


## The cacheSolve function accepts a matrix and a list produced by the makeCacheMatrix function as its
## arguments. If the inverse matrix is not null and the new matrix is equal to the cached matrix, the 
## program returns the cached inverse matrix without further calculation. If the condition is not met,
## the inverse matrix for the new matrix is calculated and stored in the cache.

cacheSolve <- function(X = matrix(), Y, ...){
#     Get the matrix and inverse from makeCacheMatrix output
      newMat <- X
      mat <- Y$get()
      invM <- Y$getInverse() 
#     Test to see if the inverse is not null and if the new matrix is equal to the matrix already in memory
      if(!is.null(invM) && identical(newMat, mat, ignore.environment = TRUE)) {
            message("Getting cached inverse matrix")
            return(invM)
      }
#     set the matrix to the new value
      Y$set(newMat)
#     if the condition above is not met, calculate the inverse of the matrix
      invM <- solve(newMat)
#     set the inverse to the new value
      Y$setInverse(invM)
      invM   
}
