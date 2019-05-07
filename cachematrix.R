# This code is for week's 3, final assignment
# The following function creates a special object which is - in reality - a matrix containing,
# a function to set and get the value of a given matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # x: an invertible matrix
  
  # initializes the variable that will store the inverse of a given matrix
  
    inv<- NULL     
    
  # define the set function to assign new
    
  set<- function(y) {       
    
    
    x <<- y                                   # assign the value of matrix in parent environment
    inv <<- NULL                              # in case of a new matrix, reset inv to NULL
  }
  get<- function()                            # definition of the get fucntion, returns value of the matrix argument
  x
  setinv<- function(inverse)
  inv <<- inverse                             # assigns value of inv in parent environment
   
  getinv<- function()                         # where is called, gets the value of inv
  inv
  
  # referencing to the functions with the $ operator
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# This function calculates the inverse of the special object matrix returned by the previous function
# In case that the inverse has already been calculated and the matrix is not changed,
# then cacheSolve will retrieve the inverse from the cache
 
cacheSolve<- function(x, ...) {              # x: output of makeCacheMatrix()
  
 
  
  inv<- x$getinv()                           # inverse of the original matrix input to makeCacheMatrix()
  
  
  # if the inverse has already been calculated
  
  if (!is.null(inv)){
    
     
    message("getting cached data")           # get it from the cache and skips the computation.
    return(inv)
  }
  
 
  mat.data<- x$get()                         # otherwise, calculates the inverse using the solve function
  inv<- solve(mat.data, ...)
  
  
  
  x$setinv(inv)                             # sets the value of the inverse in the cache via the setinv function.
  
  return(inv)
}
