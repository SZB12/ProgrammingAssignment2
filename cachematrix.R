# The makeCacheMatrix and cacheSolve functions take a matrix (matrix must be 
# invertible), then calculate and cache its inverse. If the inverse of the 
# matrix has been already cached, the inverted matrix is retrieved and is not 
# recalculated. Either way, the inverted matrix is returned. 

# The makeCacheMatrix function takes in an invertible matrix, x, and returns a 
# list of four functions and stores x and Inv, which is the inverse of x. The 
# super-assignment operator (<<-) is used to store x and Inv in the parent 
# environment so that both can be retrieved without recalculation. 

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(z) {
    x <<- z
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) Inv <<- i
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# The cacheSolve function takes a list of functions that were created by the 
# makeCacheMatrix function as arguments, then first gets the stored inverted 
# matrix, Inv. If the inverse has not been calculated, Inv is Null and it is 
# then calculated, cached and returned. If the inverted matrix has been 
# calculated previously, it is retrieved from the cache with a message and 
# returned without recalculation. 

cacheSolve <- function(cacheMatrix, ...) {
  Inv <- cacheMatrix$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data <- cacheMatrix$get()
  Inv <- solve(data, ...)
  cacheMatrix$setInv(Inv)
  Inv
}
