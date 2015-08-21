## These following functions first create a "matrix" object that can cache 
## its inverse then compute the inverse of the special "matrix." 

## This function creates a matrix object. 

makeCacheMatrix <- function(x = matrix()) { 
  ## Set inv as a variable with a NULL value 
  inv = NULL 
  
  ## Creates a new function that sets two variables. Sets x to be y and inv to 
  ## be NULL 
  set = function(y) { 
    
    x <<- y 
    inv <<- NULL 
  } 
  
  ## Returning the matrix X, setting the variable inv to be the inverse, and 
  ## getting the inverse 
  get <- function() x 
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv 
  
  ## Returns all of the functions set above. 
  list(set=set, get=get, setinv=setinv, getinv=getinv) 
} 

## This object checks to see if we already have the inverse cached. If we 
## already have the inverse cached, it returns the inverse. Otherwise, it 
## compuptes the inverse. 

cacheSolve <- function(x, ...) { 
  
  ## Sets inv to be the inv set in the above function. 
  inv <- x$getinv() 
  
  ## A function that returns cached value of inv if inv is not null. 
  if (!is.null(inv)){ 
    
    message("getting cached data") 
    return(inv) 
  } 
  
  ## Creating new variavle mat.data and assigns it to the value of the matrix 
  ## x. Takes the inverse of mat.data and sets it equal to the variable inv. 
  mat.data <- x$get() 
  inv <- solve(mat.data, ...) 
  
  
  x$setinv(inv) 
  ## Returns the inverse matrix 
  return(inv) 
}