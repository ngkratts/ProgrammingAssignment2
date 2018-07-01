#The following functions cache the inverse of an invertible square matrix (determinant != 0), 
#then return the inverse. Caching saves time for very large matrices.


#the first function, makeCacheMatrix(), caches the inverse

#Take matrix as input:
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  #set value of matrix
  setMatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Get the value of the matrix
  getMatrix = function() x
  
  #set the value of inverse matrix
  setInverse = function(inverse) invMatrix <<- inverse 
  
  #get value of inverse matrix
  getInverse = function() invMatrix
  
  
  list(setMatrix=setMatrix, 
       getMatrix=getMatrix, 
       setInverse=setInverse, 
       getInverse=getInverse)
}



## This function takes the output of the previous matrix as an input and checks if the new inverse matrix has any value in it.
#if the original matrix is empty, it gets the data from the original matrix and finds the inverse using solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  #if inverse matrix is not NULL
  if(!is.null(invMatrix)){
    message("getting cached matrix")
    return(invMatrix)
  }
  #if inverse matrix IS NULL then get original matrix data
  x <- x$getMatrix()
  
  #use solve function to invert matrix
  inv <- solve(x, ...)
  x$setInverse(inv)
  return(inv) #return inverse matrix
}


