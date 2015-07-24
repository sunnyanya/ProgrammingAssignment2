## These functions are designed to create and inverse matrix. If the inverse functions already exists, the program will
##used to cached matrix and won't calculate it twice.

## The first function, makeCacheMatrix creates a matrix, which means that we have a set and get functions as well as the set and
## get functions for the inverse matrix

makeCacheMatrix<- function(mat = matrix()) {
    i <- NULL
    
    set <- function(y) {
      mat <<- y
      i <<- NULL
    }
    
    get <- function() mat
    
    setInverse <- function(inverse) i <<- inverse 
    getInverse <- function() i
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  }


## cacheSolve function checkes if the inverse matrix was already calculated. If it was, the function will use the inverse matrix that already exists. If it doesn't 
## exits, the function will calculate it and save it. 

cacheSolve <- function(mat, ...) {
  inverse <- mat$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- mat$get()
  inverse <- solve(data)
  mat$setInverse(inverse)
  inverse
}
