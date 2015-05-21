
makeCacheMatrix <- function(x = matrix()) {
  
  x_Inv <- NULL
  set <- function(y) {
    x <<- y
    x_Inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inv) x_Inv <<- inv
  getInv <- function() x_Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) { 
    message
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m) 
  m
}