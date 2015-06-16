## Function 'makeCacheMatrix' to cache a given matrix and it's inverse
## Function 'cacheSolve' to to return inverse of given matrix

## makeCacheMatrix function creates a list to cache & return elements of a  
## matrix and it's inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i <<- matrix(NA)
  
  setMatrix <- function(y){
    x <<- y
    i <<- matrix(NA)
  }
  
  getMatrix <- function(){
    x
  }
  
  setInverse <- function(inverseMatrix){
    i <<- inverseMatrix
  }
  
  getInverse <- function(){
    i
  }
  
  list( setMatrix = setMatrix,
        getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve checks if the inverse matrix of a given matrix is already cached
## or not, if not then will calculate inverse matrix using the function solve()
## and saves that result in the list created using makeCacheMatrix() function.

cacheSolve <- function(x, ...) {
  
  i <- x$getInverse()
  
  if(identical(i,matrix(NA))){
    
    Matrxdata <- x$getMatrix()
    i <- solve(Matrxdata,...)
    x$setInverse(i)
    return(i)
    
  }
  
  message("Getting cached 'data of Matrix'") 
  i
}