## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        
        invMatrix <- NULL
  
  #setting the values of the Matrix
  setMatrix <- function(y) 
  {
          x <<- y
          invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              #getting the value of the Matrix
  setInverse <- function(inverse) invMatrix <<- inverse  #assigning the value of the invertible matrix
  getInverse <- function() invMatrix                     #getting the value of the invertible matrix
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) 
        {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
