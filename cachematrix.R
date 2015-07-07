## Creates a Matrix Object that remembers its own Inverse by caching it

## Matrix object that caches its own Inverse

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse of a given makeCacheMatrix object by way of solve()
## and caches the result

cacheSolve <- function(x, ...) 
{
  inverse <- x$getinverse()
  if(!is.null(inverse))
  {
    message("retrieve cached inverse")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
