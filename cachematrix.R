## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## makeCacheMatrix create a matrix object
makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL
          set <- function(y){
                    x <<- y 
                    inverse <<- NULL
          }
          
          get <- function() x
          
          setInverse <- function(y){
                    inverse <<- y
          }
          
          getInverse <- function() inverse  
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve returns the inverse matrix
cacheSolve <- function(x, ...) {
          inverse <- x$getInverse()
          if(!is.null(inverse)){
                    message("Inverse matrix is already calculated")
                    return(inverse)
          }
          
          matrix <- x$get()
          inverse <- solve(matrix)
          x$setInverse(inverse)
          inverse
}
