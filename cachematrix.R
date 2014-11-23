## These two functions allow for the one-time calculation of the inverse of a matrix

## makeCacheMatrix creates a list that sets or retrieves the value of the matrix,
## and also sets or retrieves the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ##To allow for storing the cached result
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ##To allow for retrieving the cached result
  get <- function()x
  
  ##Inverse of the matrix
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  ##The list
  list(set=set, get=get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of a matrix,
## or retrieves it from the cache if already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  ##Check to see if this has already been calculated
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  
  ##If not already calculated, then calculate it and store the result
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}
