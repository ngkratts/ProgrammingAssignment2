#The first function, makeCacheMatrix, creates a special "vector" which is really just a list containing a function to:
#set value of matrix
#get value of matrix
#set value of inverse of matrix
#get value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  #set value of matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #Get the value of the matrix
  get <- function() x
  
  #set the value of inverse matrix
  setInverse <- function(inverse) i <<- inverse 
  
  #get value of inverse matrix
  getInverse <- function() i
  
  #return vector
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



#This function computes the inverse of the matrix created with the above function. 
#First it checks to see if the inverse has already been calculated.
#if so, it gets the inverse and skips the computation. 
#otherwise, it computes the inverse of the matrix and sets the value of the inverse matrix in the cache via the setInverse function

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  #if inverse matrix is not NULL
  if(!is.null(i)){
    message("getting cached matrix")
    return(i)
  }
  #if inverse matrix IS NULL then get original matrix data
  matrix <- x$get()
  
  #use solve function to invert matrix
  i <- solve(matrix, ...)
  x$setInverse(i)
  i #return inverse matrix
}
