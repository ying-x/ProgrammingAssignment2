## The two functions in the followings cache the inverse of a matrix
## in the memory for repetitive call

## makeCacheMatrix is a function returns a list which contains
## the set, get, setinverse, and getinverse function

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL
      }
      get <- function(){
            x
      }
      setinverse <- function(inverse){
            i <<- inverse
      }
      getinverse <- function(){
            i
      }
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function tries to get the inverse of a matrix.
## If the inverse matrix already exists, the function will return the inverse matrix from cache; 
## Otherwise, the function will compute the inverse matrix and return the result

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      matrix <- x$get()
      i <- solve(matrix, ...)
      x$setinverse(i)
      i
}
