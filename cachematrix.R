## These functions allow user to check if the inverse of a matrix has been solved and stored in cache.
## If not, it will solve for the function and store it in cache

## creates a special matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x<<-y
    i<<-NULL
  }

  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  matrix(c(set, get, 
          setinverse, getinverse),
        nrow=2, byrow=T, 
        dimnames=list(c("matrix", "inverse"), c("set", "get")))
}


## First checks to see if inverese has been calculated. If not, calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x["inverse","get"][[1]]()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
    
  }
  data <- x["matrix","get"][[1]]()
  i <- solve(data)
  x["inverse","set"][[1]](i)
  return(i)
}
