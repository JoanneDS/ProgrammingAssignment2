## This `makeCacheMatrix` function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  set <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(Matrix) InvMatrix <<- Matrix
  getInvMatrix <- function() InvMatrix
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## This 'cacheSolve' function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
## If the inverse has already been calculated(and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvMatrix<-x$getInvMatrix()
  if(!is.null(InvMatrix)) {
    message("getting cached data")
    return(InvMatrix)
  }
  data <- x$get()
  InvMatrix <- solve(data, ...)
  x$setInvMatrix(InvMatrix)
  InvMatrix  
}
