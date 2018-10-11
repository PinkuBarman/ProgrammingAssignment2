## Functions that cache the inverse of a matrix.

## First function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL      #initialize inverse matrix, initialize to null
  set <- function(y){
    x <<- y
    inver <<- NULL   # Reset inver value to null 
  }
  get <- function() x
  setInverse <- function(solveMatrix) inver <<- solveMatrix
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
                    # Return list of all functions
}


## cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  
  inver <- x$getInverse() # assign the getinverse matrix values to inver
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }                      # if x is already in cache, return inverse matrix
  data <- x$get()        
  inver <- solve(data)
  x$setInverse(inver)
  inver 
  
  ## Return a matrix that is the inverse of 'x'
}
