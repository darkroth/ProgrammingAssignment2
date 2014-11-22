## The functions below create a matrix-like object that can store its own inverse
## in order to avoid costly computations

## Returns "matrix" object with cacheable inverse

makeCacheMatrix <- function(mtx = matrix()) {
  ##cached version of matrix Inverse
  cachedInv <- NULL
  
  ##sub-function to store matrix
  set <- function(y) { 
    mtx <<- y
    cachedInv <<- NULL
  }
  ##sub-function to return matrix
  get <- function() mtx
  ##sub-function to set inverse
  setInv <- function(y) {
    cachedInv <<- y
  }
  ##sub-function to return inverse
  getInv <- function() cachedInv
  ##list of sub-functions
  list(get = get,
       set = set,
       setInv = setInv,
       getInv = getInv)
}


## Returns a cached version of inverse if availabe, else calculates anew

cacheSolve <- function(inMatrix, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Does not use local variables, only functions of inMatrix 
  if(!is.null(inMatrix$getInv())) {
    message("getting cached data")
    return(inMatrix$getInv())
  }
  ##store inverse of matrix
  inMatrix$setInv(solve(inMatrix$get()))
  ##return inverse of matrix
  inMatrix$getInv()
}
